data <- data %>%
  mutate(cbcat = case_when(
    cbcount == 0 ~ "0",
    cbcount == 1 ~ "1",
    cbcount == 2 ~ "2",
    cbcount >= 3 ~ "3+"
  ))

row1 <- c(table(data$cbcat),
          table(data$newcb),
          sum(!is.na(data$attempt), na.rm=T))
row2 <- c(table(data$cbcat[data$attempt==0]),
          table(data$newcb[data$attempt==0]),
          sum(data$attempt==0, na.rm=T))
row3 <- paste0(round(((row1-row2)/row1)*100,0),"%")
row4 <- c(table(data$cbcat[data$attempt==1]),
          table(data$newcb[data$attempt==1]),
          sum(data$attempt==1, na.rm=T))
row5 <- c(table(data$cbcat[data$success==0]),
          table(data$newcb[data$success==0]),
          sum(data$success==0, na.rm=T))
row6 <- c(table(data$cbcat[data$success==1]),
          table(data$newcb[data$success==1]),
          sum(data$success==1, na.rm=T))
row7 <- paste0(round(((row4-row5)/row4)*100,0),"%")

descriptives <- rbind(row1, row2, row3, row4, row5, row6, row7)
descriptives <- as.data.frame(descriptives)
rownames(descriptives) <- c("Total observations",
                            "No coup attempt",
                            "% country years with attempts",
                            "Coup attempts",
                            "Failed",
                            "Successful",
                            "% coup attempts successful")


rows_to_format <- c(1,2,4,5,6)

descriptives[rows_to_format, ] <- lapply(
  descriptives[rows_to_format, ],
  function(col) {
    numeric_col <- as.numeric(col)
    format(numeric_col, big.mark = ",", scientific = FALSE)
  }
) %>% as.data.frame()

kable(descriptives,
      align = "r",
      linesep = "",
      caption = "Counterbalancing and Coups",
      col.names = c("0",
                    "1",
                    "2",
                    "3+",
                    "No",
                    "Yes",
                    "Total")) %>%
  add_header_above(c(" ","Number of Counterweights"=4,"New Counterweight"=2,"")) %>%
  save_kable("table1.html")
