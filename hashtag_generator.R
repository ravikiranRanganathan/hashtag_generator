# deal with the json object response
# install.packages('jsonlite')
# install.packages("stringr")
library(jsonlite)
library('stringr')

options(max.print=999999)

# function should take in a hashtag, and download the corresponding webpage information and pass it to clean_tags()
# input: hashtag - the user supplied hashtag used to get the list of hashtags
download_page_text = function(hashtag){

  # create the url that the data exists at
  final_url = paste0("https://www.instagram.com/explore/tags/",hashtag,"/?__a=1" )

  # read text from the page into page_data
  page_data = readLines(final_url)

  return(clean_tags(page_data))
}

# function to take the page string and return a list of clean hashtags
clean_tags = function(page_data){
  # convert text to lowercase, for easier regex/pattern extraction and matching
  lower_text = tolower(page_data)

  # add space before # for better pattern extraction
  page_text = str_replace_all(lower_text, "#", " #")

  # replace
  page_text = gsub("\\\\","   ",page_text)

  # extract all hashtags from the string into a list structure
  all_tags = str_extract_all(page_text, "#\\S+")

  # get the first list element - i.e. all the hashtags
  hashtag_list = all_tags[[1]]

  return(hashtag_list)
}

# the list of tags, and returns a table of sorted tags by freq
list_to_table = function(hashtag_list, start_ind, end_ind){
  sorted_table = sort(table(hashtag_list), decreasing = T)
  return(sorted_table[start_ind:end_ind])
}

# take in the df and get a usable list of level 1 tags
get_tags = function(sorted_hastag_table){
  df = as.data.frame(sorted_hastag_table)
  tags = df[,1]
  tags = as.vector(tags)
  return(tags)
}

get_level_two_tags = function(level_one_tags){
  level_two_tags = c()
  for(tag in level_one_tags){

    l = nchar(tag)
    tag = substr(tag, 2, l)

    individual_hashtag_list = download_page_text(tag)

    print("Processing tag: ")
    print(tag)

    sorted_hashtag_table_one = list_to_table(individual_hashtag_list, 2, 20)

    df = as.data.frame(sorted_hashtag_table_one)
    tags = as.vector(df[,1])

    flag = 0 # number added to list
    ind = 1 # index in list

    lim = length(tags)

    while(flag < 2 & ind < lim){
      if((tags[ind] %in% level_two_tags | (length(level_two_tags) == 20)
          | tags[ind] %in% level_one_tags | nchar(tags[ind]) < 3 ) | is.na(tags[ind])){

      } else {
        level_two_tags = c(level_two_tags, tags[ind])
        flag = flag + 1
      }

      if(ind == lim){
        print(paste0("Insufficient information found for: ", tag))
      }

      ind = ind + 1
    }

  }
  return(level_two_tags)
}













# get the list of all the corresponding tags for this base tag
hashtag_list = download_page_text("shelbycobra")

# return a table of the 10 most frequent hashtags other than our base tag
sorted_hastag_table = list_to_table(hashtag_list, 2, 11)

# get all the level one tags in a list
level_one_tags = get_tags(sorted_hastag_table)

# get the level two tags
level_two_tagss = get_level_two_tags(level_one_tags)

print(paste0("Level one tags ", level_one_tags))
print(paste0("Level two tags ", level_two_tagss))

final_vector = c(level_one_tags, level_two_tagss)

output_string = paste(final_vector, sep="", collapse=" ")
output_string

output_string = paste(level_one_tags, sep="", collapse=" ")

