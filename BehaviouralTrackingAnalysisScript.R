#R Script for the research on behavioral tracking.

#Created by: Shiva Saxena
#Date Started: 08-May-2018

#Real Date Of Finishing :


#import the necessory libraries required
install.packages("dplyr", dep = TRUE);
install.packages("ggplot2", dep = TRUE);
install.packages("rjson", dep = TRUE);
install.packages("tidyverse", dep = TRUE);
install.packages("rlist", dep = TRUE);


library("dplyr");
library("ggplot2");
library("rjson");
library("tidyverse");
library("rlist");

#Loading RAW JSON data collected for the websites into vaiable.
raw.data <-
  fromJSON(file = "lightbeamData.json",
           method = "C",
           unexpected.escape = "error")



convertToList <- function(x) {
  #clean and rename the function
  returnList <- list()
  i = 1
  for (site in x) {
    returnList[[i]] <- site
    i <- i + 1
  }
  return(returnList)
}

processed.data.list <- raw.data %>%
  convertToList();


processed.data.frame <- data.frame(Reduce(rbind, processed.data.list));

total.visited.sites <- processed.data.list %>%
  filter(firstParty == TRUE);

total.non.visited.sites <- processed.data.frame %>%
  filter(firstParty == FALSE);

websites.stats <- data.frame(
  variable = c("Visited Websites", "Non-visited websites"),
  value = c(
    length(total.visited.sites[[1]]),
    length(total.non.visited.sites[[1]])
  )
)
websites.stats$percent <-
  paste(round(websites.stats$value / sum(websites.stats$value) * 100, 1), "%", sep =
          "")


blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold")
  )


pie<- ggplot(websites.stats, aes(x = "", y = value, fill = variable)) +
  geom_bar(width = 1, stat = "identity",color='black') +
  geom_text(aes(y = value / 2 + c(0, cumsum(value)[-length(value)]), label = percent), size =
              4) +
  labs(title = "Visited vs Non-Visited sites");

pie <- pie +coord_polar(theta='y');

pie <- pie+ 
  # remove black diagonal line from legend
  guides(fill=guide_legend(override.aes=list(colour=NA))) +
  blank_theme+
  theme(axis.text.x=element_blank());

pie <- pie +
  # prettiness: make the labels black
  theme(axis.text.x=element_text(color='black')) +
  scale_y_continuous(
    breaks=cumsum(websites.stats$value) - websites.stats$value/2,   # where to place the labels
    labels=websites.stats$value # the labels
  )
print(pie);




# find the top 10 websites that have the most number of third connections

third.party.connection
processed.data %>%
  








total.visited.sites <- getVisitedSites(raw.data)






###########################################################################################################

calls.to.nonvisited.sites <-  filter(raw.data,
                                     visited == FALSE,
                                     (source != target),
                                     contentType %in% useful.content.types) %>%
  group_by(target) %>%
  summarise(times.call.made = n()) %>%
  arrange(desc(times.call.made)) %>%
  mutate(relative.times.call.made = round(times.call.made / sum(times.call.made) * 100, 2)) %>%
  top_n(n = 25)



#Plotting a barchart for the top 10 web sites to which the call was made
# So from this we can infer that the top websites to which data is being shared are ad networks/social networks or not.
ggplot(data = calls.to.nonvisited.sites, aes(x = target,
                                             y = relative.times.call.made,
                                             fill = target)) +
  theme(axis.text.x = element_blank(), axis.ticks = element_blank()) +
  xlab("websites") +
  ylab("% value of calls made") +
  geom_bar(
    width = 0.8,
    colour = "black",
    stat = "identity",
    position = "dodge"
  ) +
  scale_fill_brewer(palette = "Set3") +
  geom_text(
    aes(label = relative.times.call.made),
    vjust = 1.6,
    color = "Black",
    size = 3.5
  )


###########################################################################################################
#non visited websites that store a cookie on users machine
non.visited.websites.with.cookie <-
  filter(non.visited.websites, cookie == TRUE) %>%
  select(website, cookie)

websites.stats <- data.frame(
  variable = c("Without Cookie", "With Cookie"),
  value = c(
    length(non.visited.websites.with.cookie$website),
    length(non.visited.websites$website)
  )
)
websites.stats$percent <-
  paste(round(websites.stats$value / sum(websites.stats$value) * 100, 1), "%", sep =
          "")


ggplot(websites.stats, aes(x = "", y = value, fill = variable)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = pi / 3) +
  geom_text(aes(y = value / 2 + c(0, cumsum(value)[-length(value)]), label = percent), size = 3.5) +
  labs(title = "Third party websites storing cookie on uses machine")


###########################################################################################################
# find that on an average how many web sites third party calls are made by web sites
avg.third.party.calls <-
  length(non.visited.websites$website) / length(visited.websites$website)

print(avg.third.party.calls)


#########################################################################################################
# find the list of non visited websites and there connections to visited websites

third.party.connections <-
  select(raw.data, source, target, contentType) %>%
  filter((source != target)) %>%
  distinct()


number.of.connection.first.to.third.party <-
  select(third.party.connections, website = source) %>%
  group_by(website) %>%
  summarise(number.of.connections = n()) %>%
  arrange(desc(number.of.connections))


number.of.connection.third.to.first.party <-
  select(third.party.connections, website = target) %>%
  group_by(website) %>%
  summarise(number.of.connections = n()) %>%
  arrange(desc(number.of.connections))


#################################################################################################
# how many of the non visited websites Created or Updated some content on on to there servers
third.party.create.update.req <-
  select(raw.data, source, target, contentType, method) %>%
  filter(source != target, method %in% c("POST", "PUT"))
