build_sparkline <- function(nested_values){
  
  if (pluck(nested_values,"close")[1] >= tail(pluck(nested_values,"close"),1)){
    color = "red"
    fillColor = "#680000" #"#800000" #"#ff020282"
    #3a0000
  }else{
    color = "#0ddd0d" #green"
    fillColor = "#193b05c4"
  }
  
  spk_composite(sparkline(pluck(nested_values,"close"),
                          height = "50px",
                          width = "150",
                          fillColor = fillColor,
                          type = "line",
                          lineColor = color,
                          lineWidth = 2,
                          chartRangeMin = min(pluck(nested_values,"close")),
                          chartRangeMax = max(pluck(nested_values,"close"))
  ))
}



build_sparkline_2 <- function(nested_values){
  
  if (pluck(nested_values,"close")[1] >= tail(pluck(nested_values,"close"),1)){
    color = "red"
    fillColor = "#680000"
    #3a0000
  }else{
    color = "#0ddd0d" #green"
    fillColor = "#193b05c4"
  }
  
  spk_composite(sparkline(pluck(nested_values,"close"),
                          height = "50px",
                          width = "150",
                          fillColor = fillColor,
                          type = "line",
                          lineColor = color,
                          lineWidth = 2,
                          chartRangeMin = min(pluck(nested_values,"close")),
                          chartRangeMax = max(pluck(nested_values,"close")),
                          tooltipFormatter = htmlwidgets::JS(
                            sprintf(
                              "function(sp, options, fld){debugger;return %s[fld.x] + '<br/>';}",
                              jsonlite::toJSON(
                                #paste(c(1,3,5,3,8),
                                format(
                                  paste(pluck(nested_values,"close"),pluck(nested_values,"date"), sep = "<br>")
                                )
                              )
                            )
                            
                          )
                          )
                )
}



volume_sparkline <- function(nested_values){
  sparkline(pluck(nested_values,"volume"),
            height = "50px",
            width = "150",
            fillColor = "black",
            type = "line",
            lineColor = "#ffc00d",
            lineWidth = 2,
            chartRangeMin = min(pluck(nested_values,"volume")),
            chartRangeMax = max(pluck(nested_values,"volume"))
  )
}

stock_delta <- function(value){
  if(value<0){
    tag_type <- "tag num-neg"
  }else if(value>0){
    tag_type <- "tag num-pos"
  }
  else if (value==0){
    tag_type <- "tag num-zero"
  }
  
  if(value<0){
    prefix <- "-"
  }else if(value>0){
    prefix <- "+"
  }
  else if (value==0){
    prefix <- " "
  }
  
  div(class = tag_type, paste0(prefix,100 * abs(value)," %"))
  
}




convert_lag <- function(gdf,end_date){
  lag <- str_to_lower(unique(gdf$lag)[1])
  if (str_detect(lag,"week")){
    start_date <- end_date %m+% weeks(-1 * str_split(lag, " ")[[1]][1] %>% as.integer())
  }else if(str_detect(lag,"month")){
    start_date <- end_date %m+% months(-1 * str_split(lag, " ")[[1]][1] %>% as.integer())
  }
  else if(str_detect(lag,"year")){
    start_date <- end_date %m+% years(-1 * str_split(lag, " ")[[1]][1] %>% as.integer())
  }
  gdf$start_date <- rep(start_date,dim(gdf)[1])
  gdf
}

