# uploading needed libraries
library(vkR)
library(tidyverse)


vkOAuth(<put your client_id here>, 'friends, groups')

setAccessToken(access_token = <put your access_token here>)

walls_collection = function(groups, attachments = F, reposts = F){
  
  # this function is a wrapping for the vkR function getWallExecute that unlists all needed data and returns a dataframe
  # with the help of this function, we can extract all the main information for the wall post: id of the post, 
  # id of the author(from_id), id of the group (owner_id), date, text, count of likes, comments, and reposts
  # the function also extracts
  #
  # Attributes:
  # groups (numeric vector): vector of ids of the groups (should be written without '-' in the beginning)
  # attachments (boolean): if True, adds the information for attachments to the posts (type, link, title)
  # reposts (boolean): if True and a certain post is a repost from another group or users, function returns info 
  # on the original post (in the final dataframe, the info on original posts starts with 'copy_history')
  
  walls = data.frame()
  
  for (gr in groups){
    tryCatch({
      group_id = as.character(-gr)
      print(str_c('collecting wall for ', group_id))
      wall <- getWallExecute(owner_id=group_id, count=0, use_db = F, progress_bar = T)
      
      wal_df = data.frame(wall$posts$id, wall$posts$from_id, wall$posts$owner_id, wall$posts$date, wall$posts$text,
                          wall$posts$likes$count, wall$posts$comments$count, wall$posts$reposts$count)
      
      colnames(wal_df)[1:8] = c('id', 'from_id', 'owner_id', 'date', 'text', 'likes.count', 'comments.count', 'reposts.count')
      
      # unlisting attachments 
      
      if ('attachments' %in% colnames(wall$posts) & attachments = T){
        
        attachments = wall$posts$attachments %>% 
          map(unlist) %>%
          map_if(is.null, as.character) %>%
          map(t) %>% 
          map(as.data.frame) %>%
          map(~add_column(.,temp = NA, .name_repair = 'minimal'))  %>% # this row is needed for the posts that do not have attachments
          bind_rows() %>%
          select(matches('^type|link.url|link.title')) 
        
        colnames(attachments) = str_c('attachments.', colnames(attachments))
        wal_df = cbind(wal_df, attachments)
      }
      
      # unlisting reposts
      
      if ('copy_history' %in% colnames(wall$posts) & reposts = T){
        
        copy_history = wall$posts$copy_history %>% 
          map(unlist) %>%
          map_if(is.null, as.character) %>%
          map(t) %>% 
          map(as.data.frame) %>%
          map(~add_column(.,temp = NA, name_repair = 'minimal')) %>%
          bind_rows() %>%
          select(matches('^id|^owner_id|^from_id|^date|^post_type|^text|attachments.link.url|attachments.link.title')) 
        
        colnames(copy_history) = str_c('copy_history.', colnames(copy_history))
        wal_df = cbind(wal_df, copy_history)
      }
      walls = bind_rows(walls, wal_df)
    }, error=function(e){
      print(paste("MY ERROR: ", e))})
  }
  
  return(walls)
}

comments_collection = function(walls, ){
  # this function is a wrapping for the vkR function postGetComments that unlists all needed data and returns a dataframe
  # with the help of this function, we can extract all the main information for the wall post: id of the post, 
  # id of the author(from_id), id of the group (group), date, text, the count of likes
  #
  # Attributes:
  # groups (dataframe): dataframe returned by the function walls_collection
  # attachments (boolean): if True, adds the information for attachments to the comments (type, link, title)

  
  comments = data.frame()
  
  for (o_id in unique(walls$owner_id)){
    
    wal_df = walls %>% 
      filter(owner_id == o_id & comments.count > 0)
    
    if(nrow(wal_df) > 0){
      for (pid in wal_df$id){
        tryCatch({
          comm = postGetComments(owner_id = o_id, post_id = pid, need_likes = 1, count = 50)
          
          if (comm$count > 0){
            print(str_c('collecting comments of ', o_id, ' for ', pid))
            com_df = data.frame(comm$comments$from_id, comm$comments$date, comm$comments$text, 
                                comm$comments$likes$count)
            colnames(com_df) = c('from_id','date', 'text', 'likes.count')
            com_df$group = rep(-o_id, nrow(com_df))
            com_df$post_id = rep(pid, nrow(com_df))
            
            if ('attachments' %in% colnames(comm$comments) & attachments = T){
              
              attachments = comm$comments$attachments %>% 
                map(unlist) %>%
                map_if(is.null, as.character) %>%
                map(t) %>% 
                map(as.data.frame) %>%
                map(~add_column(.,binding_col = NA, .name_repair = 'minimal')) %>%
                bind_rows() %>%
                select(matches('^type|link.url|link.title')) 
              
              colnames(attachments) = str_c('attachments.', colnames(attachments))
              com_df = cbind(com_df, attachments)
            }
            comments = bind_rows(comments, com_df)
          }
        }, error=function(e){
          print(paste("MY ERROR: ", e))})
      }
    }
  }
  return(comments)
}
