#
library(extrafont)

#
library(foreign)
library(car)
library(dplyr) 
library(ggplot2)
library(igraph)
library(stringr)
library(reshape)
library(ggraph)
library(tidyverse)
library(network)
library(sna)
library(ndtv)
library(RColorBrewer)
##################################################################################################

#FOR NICE GRAPHS
APA.FORMAT = theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(),
        text = element_text(family = 'Times'))
##################################################################################################

#Check files
getwd
setwd("/Users/User/Desktop/xxx.archive1")
list.files()


#select only files with "rollcall" in the name
voting_data <- list.files()[grep("rollcall", list.files())]
voting_data

#FIRST CHECK WITH ONE SESSION ONLY!
z <- 1
voting_data[z]
s1 <- read.csv(voting_data[z],header=TRUE,strip.white=TRUE,encoding = 'UTF-8')

s1[1:6,1:6]

bills <- read.csv("/Users/User/Desktop/xxx.archive1/bills_convoc8_voted.csv",header=TRUE,strip.white=TRUE,encoding = 'UTF-8') 

bills_selected <- bills[bills$event_result=="positively",]


s1 <- s1[order(s1$full_name),]
s1[s1==""] <- NA
s1[s1=="n/a"] <- NA
s1 <- s1[complete.cases(s1$full_name),]

############################################################################
### MPs information cleaning, ordering, coding

{mps_convoc8 <- read.csv("mps_convoc8.csv",header=TRUE,strip.white=TRUE,encoding = 'UTF-8')
mps_convoc8[mps_convoc8==""] <- NA #a lot of empty values - to NA
mps_convoc8[mps_convoc8=="n/a"] <- NA #a lot of empty values - to NA
mps_convoc8$faction[mps_convoc8$date_resignation=="02.12.2014"] <-NA
mps_convoc8 <- mps_convoc8[complete.cases(mps_convoc8$name_eng), ]
mps_convoc8 <- mps_convoc8[complete.cases(mps_convoc8$faction), ]
mps_convoc8 <- mps_convoc8[complete.cases(mps_convoc8$X.U.FEFF.id), ]#exclude NAs
s1 <- s1[s1$full_name %in% (mps_convoc8$name_ukr),]
mps_convoc8 <- mps_convoc8[mps_convoc8$name_ukr %in% (s1$full_name),]
row.names(s1) <- c(1:419)
mps_convoc8 <- mps_convoc8[order(mps_convoc8$name_ukr),]
mps_convoc8 <- tibble::rowid_to_column(mps_convoc8, "ID")
mps_convoc8$name_ukr <- factor(mps_convoc8$name_ukr)

mps_convoc8$faction[mps_convoc8$party == unique(mps_convoc8$party)[1]] <- unique(mps_convoc8$faction)[1]
mps_convoc8$faction[mps_convoc8$party == unique(mps_convoc8$party)[2]] <- unique(mps_convoc8$faction)[2]
mps_convoc8$faction[mps_convoc8$party == unique(mps_convoc8$party)[5]] <- unique(mps_convoc8$faction)[3]
mps_convoc8$faction[mps_convoc8$party == unique(mps_convoc8$party)[4]] <- unique(mps_convoc8$faction)[4]
mps_convoc8$faction[mps_convoc8$party == unique(mps_convoc8$party)[7]] <- unique(mps_convoc8$faction)[5]

mps_convoc8$coalition <- "opposition"
mps_convoc8$coalition[mps_convoc8$faction=="Petro Poroshenko Block"] <- "coalition"
mps_convoc8$coalition[mps_convoc8$faction=="Radical Party of Oleg Lyashko"] <- "coalition"
mps_convoc8$coalition[mps_convoc8$faction==unique(mps_convoc8$faction)[1]] <- "coalition"
mps_convoc8$coalition[mps_convoc8$faction==unique(mps_convoc8$faction)[2]] <- "coalition"
mps_convoc8$coalition[mps_convoc8$faction==unique(mps_convoc8$faction)[5]] <- "coalition"

mps_convoc8$f.ukr[mps_convoc8$faction==unique(mps_convoc8$faction)[1]] <- "Батьківщина"
mps_convoc8$f.ukr[mps_convoc8$faction==unique(mps_convoc8$faction)[2]] <- "Народний фронт"
mps_convoc8$f.ukr[mps_convoc8$faction==unique(mps_convoc8$faction)[3]] <- "Блок Петра Порошенка"
mps_convoc8$f.ukr[mps_convoc8$faction==unique(mps_convoc8$faction)[4]] <- "РП Олега Ляшка"
mps_convoc8$f.ukr[mps_convoc8$faction==unique(mps_convoc8$faction)[5]] <- "Самопоміч"
mps_convoc8$f.ukr[mps_convoc8$faction==unique(mps_convoc8$faction)[6]] <- "Опозиційний блок"
mps_convoc8$f.ukr[mps_convoc8$faction==unique(mps_convoc8$faction)[8]] <- "Воля народу"
mps_convoc8$f.ukr[mps_convoc8$faction==unique(mps_convoc8$faction)[9]] <- "Відродження"
mps_convoc8$f.ukr[mps_convoc8$faction==unique(mps_convoc8$faction)[7]] <- "позафракційні"
unique(mps_convoc8$faction)[6]

mps_convoc8$f.id[mps_convoc8$f.ukr=="Батьківщина"] <- 1
mps_convoc8$f.id[mps_convoc8$f.ukr=="Народний фронт"] <- 2
mps_convoc8$f.id[mps_convoc8$f.ukr=="Блок Петра Порошенка"] <- 3
mps_convoc8$f.id[mps_convoc8$f.ukr=="РП Олега Ляшка"] <- 4
mps_convoc8$f.id[mps_convoc8$f.ukr=="Самопоміч"] <- 5
mps_convoc8$f.id[mps_convoc8$f.ukr=="Опозиційний блок"] <- 6
mps_convoc8$f.id[mps_convoc8$f.ukr=="Воля народу"] <- 7
mps_convoc8$f.id[mps_convoc8$f.ukr=="Відродження"] <- 8
mps_convoc8$f.id[mps_convoc8$f.ukr=="позафракційні"] <- 9
mps_convoc8$f.id <- as.integer(mps_convoc8$f.id)

leg.txt <- c("Батьківщина","Народний фронт","Блок Петра Порошенка",
             "РП Олега Ляшка","Самопоміч","Опозиційний блок",
             "Воля народу","Відродження","позафракційні")
colrs <- c("darkmagenta","orange","darkred","hotpink","green2",
           "blue","lightcyan4","chocolate4","gray0")}



s1<-s1[,grepl("\\d", colnames(s1))]#Save only columns with bills (we do not need ID, Name, party)
s1 <- s1 %>% mutate_if(is.factor, as.character)

table(bills$event_result)

colnames(s1) <- str_replace(colnames(s1), "X", "")
colnames(s1) <- str_replace(colnames(s1), "[.]", "|")

s1_ok <- s1[,colnames(s1) %in% bills_selected$merge_votes]


s1[s1=="3.Abstain"] <- "2.No"
s1[s1=="4.Did not vote"] <- "2.No"
s1[s1=="5.Absent"] <- "3.Absent"
s1

dim(s1)[2] # there are XXXX columns (bills) in session 1
           # if ALL of the values in a column is NA - then this is bad, we don't need it

listNA  <- NA
n.empty <- NA
for(i in 1:dim(s1)[2]){
  n.empty[i] <- sum(is.na(s1[,i]))
  listNA[i]  <- sum(is.na(s1[,i]))==dim(s1)[1] #IF TRUE = all are NA
}
n.empty       #equal N of epty? 86.83.57??? WHY? interesting...
table(listNA) #No TOTAL EMPTY - good

#MPS WITH ZERO ACTIVITY
missT <- apply(s1,2,function(x) is.na(x)) #Count SUM of NA
sumMT <- apply(missT,1,sum) #NA per person
table(sumMT)

s1.names <- read.csv(voting_data[z],encoding = 'UTF-8') 
dim(s1.names)[2]
s1.names[s1.names==""] <- NA #a lot of empty values - to NA

M <- max(sumMT)
s1.names$full_name[sumMT==M]  #WHO ARE THESE PEOPLE WITH ZERO ACTIVITY
corrected <- s1.names[s1.names$full_name[sumMT!=M],]
corrected <- corrected[corrected$full_name %in% (mps_convoc8$name_ukr),]

dim(corrected)
dim(s1.names)
##################################################################################################

#REPEAT THE SAME IN THE FUNCTION FOR 10 SESSIONS

#Step 1. Create the empty list "final_graphs"
#Step 2. Fill in this list with voting (10 sessions = list of length 10)



final_graphs <- list()
final_graphs_ok <- list()

for(z in 1:10){
  
  s <- read.csv(voting_data[z],header=TRUE,strip.white=TRUE,encoding = 'UTF-8')
  s <- s[order(s$full_name),]
  s[s==""] <- NA
  s[s=="n/a"] <- NA
  s <- s[complete.cases(s$full_name),]
  s <- s[s$full_name %in% (mps_convoc8$name_ukr),]

  s <- s[,grepl("\\d", colnames(s))] 
  s <- s %>% mutate_if(is.factor, as.character)
  s[s=="3.Abstain"] <- "2.No"
  s[s=="4.Did not vote"] <- "2.No"
  s[s=="5.Absent"] <- "3.Absent"

  colnames(s) <- str_replace(colnames(s), "X", "")
  colnames(s) <- str_replace(colnames(s), "[.]", "|")
  
  s.ok <- s[,colnames(s) %in% bills_selected$merge_votes]
  
  listNA  <- NA
  n.empty <- NA
  for(i in 1:dim(s)[2]){
    listNA[i]  <- sum(is.na(s[,i]))==dim(s)[1] #IF TRUE = all are NA
  }
  
  row.names(s) <- c(1:419)
  s <- s[,!listNA]
  final_graphs_ok[[z]] <- s.ok
  final_graphs[[z]] <- s
}

length(final_graphs)
final_graphs #this is a list with 10 clean voting outcomes
dim(final_graphs[[1]])

apply(final_graphs[[1]], 1, mean)

?lapply

lapply(final_graphs,dim)
##################################################################################################

#HOW much similarities in a session?
sim_a_session <- final_graphs[[1]]
dim(sim_a_session)
sim_a_session <- sim_a_session[complete.cases(sim_a_session),]
sims_1_bill <- outer(sim_a_session[,1], sim_a_session[,1], FUN = "==")

dim(sims_1_bill) #440 per 440 matrix. - similarities by column 1.
gc()
dim(sim_a_session)[2]

save_each_column <- list()
for(i in 1:196){
  save_each_column[[i]] <- outer(sim_a_session[,i], sim_a_session[,i], FUN = "==")
}

#SUM UP save_each_column
sims_all_bills <- Reduce('+', save_each_column)

sims_all_bills_weight <- sims_all_bills/dim(sim_a_session)[2]
summary(sims_all_bills_weight, na.rm=T)
#
sims_all_bills_weight[,378] #NA - this is the empty MP

sims_per_s[[1]] <- sims_all_bills_weight

na.omit(sims_per_s[[1]])

apply(sims_all_bills_weight, 1, mean, na.rm=T)

##################################################################################################

#REPEAT FOR ALL 10 sessions

sims_1_bill <- list()
sims_per_s  <- list()


for(i in 1:10) {
    sim_a_session <- final_graphs[[i]]
    n.columns <- dim(sim_a_session)[2]
    sims_1_bill <- list()
         for(j in 1:n.columns){
           sims_1_bill[[j]] <- outer(sim_a_session[,j], sim_a_session[,j], FUN = "==")
                  }

    sims_all_bills <- Reduce('+', sims_1_bill)
    sims_all_bills_weight <- sims_all_bills/n.columns

   sims_per_s[[i]] <- sims_all_bills_weight
   #stopifnot(sims_per_s[[i]] <=1) 
}

sim <- apply(sims_per_s[[3]], 1, mean, na.rm=T)
summary(sim)
hist(sim)

lapply(sims_per_s, dim)
sims_per_s[[4]][1:5,1:5]
sims_per_s[[10]][1:5,1:5]
#

##################################################################################################

#TRY WITH SESSION 1
test.graph <- sims_per_s[[1]]

test.graph
head(test.graph)

test.graph[test.graph<0.3] <- 0
test.graph[test.graph>0.3] <- 1

{res_ses_g <- graph_from_adjacency_matrix(test.graph, mode = c("undirected"))
res_ses_g <- igraph::simplify(res_ses_g, remove.multiple=FALSE, remove.loops=TRUE)
igraph::vertex.attributes(res_ses_g)$faction<-mps_convoc8$faction
igraph::vertex.attributes(res_ses_g)$ID<-mps_convoc8$ID
igraph::vertex.attributes(res_ses_g)$name_ukr<-mps_convoc8$name_ukr
igraph::vertex.attributes(res_ses_g)$coalition<-mps_convoc8$coalition
igraph::vertex.attributes(res_ses_g)$f.ukr<-mps_convoc8$f.ukr
V(res_ses_g)$label <- NA
V(res_ses_g)$size <- 5
E(res_ses_g)$width <- 1
V(res_ses_g)[f.ukr=="Батьківщина"]$color <- "darkmagenta"
V(res_ses_g)[f.ukr=="Народний фронт"]$color <- "orange"
V(res_ses_g)[f.ukr=="Блок Петра Порошенка"]$color <- "darkred"
V(res_ses_g)[f.ukr=="РП Олега Ляшка"]$color <- "hotpink"
V(res_ses_g)[f.ukr=="Самопоміч"]$color <- "green2"
V(res_ses_g)[f.ukr=="Опозиційний блок"]$color <- "blue"
V(res_ses_g)[f.ukr=="Воля народу"]$color <- "lightcyan4"
V(res_ses_g)[f.ukr=="Відродження"]$color <- "chocolate4"
V(res_ses_g)[f.ukr=="позафракційні"]$color <- "gray0"
V(res_ses_g)$shape <- "circle"
res_ses_g <-igraph::delete.vertices(res_ses_g, V(res_ses_g)[ igraph::degree(res_ses_g)==0 ])}


#igraph::vertex.attributes(res_ses_g)
#igraph::edge.attributes(res_ses_g)


plot(res_ses_g) 


##################################################################################################
#### graphs of rollcall per session with minimal threshold

fin.graph_0.3 <- list()
metrics_table_0.3 <- list()
grouped_metrics_0.3 <- list()
only_coalition_0.3 <- list()
only_opposition_0.3 <- list()
dens_list_0.3 <- list()

#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.2369  0.4271  0.4780  0.4599  0.5112  0.5560

for(i in 1:10){
  fin.m <- sims_per_s[[i]]
  
  fin.m[fin.m<0.2369] <- 0
  fin.m[fin.m>0.2369] <- 1
  
  fin.g <- graph_from_adjacency_matrix(fin.m, mode = c("undirected"))
  fin.g <- igraph::simplify(fin.g, remove.multiple=FALSE, remove.loops=TRUE)
  igraph::vertex.attributes(fin.g)$faction<-as.character(mps_convoc8$faction)
  igraph::vertex.attributes(fin.g)$ID<-mps_convoc8$ID
  igraph::vertex.attributes(fin.g)$name_ukr<-as.character(mps_convoc8$name_ukr)
  igraph::vertex.attributes(fin.g)$coalition<-mps_convoc8$coalition
  igraph::vertex.attributes(fin.g)$f.ukr<-mps_convoc8$f.ukr
  V(fin.g)$label <- NA
  V(fin.g)$size <- 5
  E(fin.g)$width <- 1
  V(fin.g)[f.ukr=="Батьківщина"]$color <- "darkmagenta"
  V(fin.g)[f.ukr=="Народний фронт"]$color <- "orange"
  V(fin.g)[f.ukr=="Блок Петра Порошенка"]$color <- "darkred"
  V(fin.g)[f.ukr=="РП Олега Ляшка"]$color <- "hotpink"
  V(fin.g)[f.ukr=="Самопоміч"]$color <- "green2"
  V(fin.g)[f.ukr=="Опозиційний блок"]$color <- "blue"
  V(fin.g)[f.ukr=="Воля народу"]$color <- "lightcyan4"
  V(fin.g)[f.ukr=="Відродження"]$color <- "chocolate4"
  V(fin.g)[f.ukr=="позафракційні"]$color <- "gray0"
  V(fin.g)$shape <- "circle"
  fin.g <- igraph::delete.vertices(fin.g, V(fin.g)[ igraph::degree(fin.g)==0 ])
  
  igraph::vertex.attributes(fin.g)$degree<-igraph::degree(fin.g)
  igraph::vertex.attributes(fin.g)$degree_norm<-igraph::degree(fin.g,normalized = T)

  table<-data.frame( N = c(table(vertex.attributes(fin.g)$f.ukr)), 
                     degree=tapply(vertex.attributes(fin.g)$degree,vertex.attributes(fin.g)$f.ukr,mean),
                     degree_norm=tapply(vertex.attributes(fin.g)$degree_norm,vertex.attributes(fin.g)$f.ukr,mean))
  table1<-data.frame( N = c(table(vertex.attributes(fin.g)$coalition)),
                      degree=tapply(vertex.attributes(fin.g)$degree,vertex.attributes(fin.g)$coalition,mean),
                      degree_norm=tapply(vertex.attributes(fin.g)$degree_norm,vertex.attributes(fin.g)$coalition,mean))
  metrics_table_0.3[[i]] <- table
  grouped_metrics_0.3[[i]] <- table1
  
  only_coal <- fin.g
  only_opp <- fin.g
  only_coal <-igraph::delete.vertices(only_coal, V(only_coal)[V(only_coal)$coalition=="opposition"])
  only_opp <-igraph::delete.vertices(only_opp, V(only_opp)[V(only_opp)$coalition=="coalition"])
  
  density_table <- data.frame( sesion = i, 
                               all_edges = gsize(fin.g), coal_edges = gsize(only_coal), 
                               opp_edges = gsize(only_opp), delta_edges = gsize(fin.g) - gsize(only_coal) - gsize(only_opp),
                               N_only_col = sum(table(vertex.attributes(only_coal)$f.ukr)),
                               overall_dense = edge_density(fin.g),
                               coal_dense = edge_density(only_coal), opp_dense = edge_density(only_opp),
                               laws_count = dim(final_graphs[[i]])[2]
                                                              )
  
  
  only_coalition_0.3[[i]] <- only_coal
  only_opposition_0.3[[i]] <- only_opp
  dens_list_0.3[[i]] <- density_table
  fin.graph_0.3[[i]] <- fin.g
}


dens_dens_0.3 <- do.call(rbind,dens_list_0.3)

#########################################################################################
#### degree destribution for factions, coalition, oposition


library(data.table)
dens_thresh_0.3 <- data.frame(matrix(ncol=4,nrow=10, 
                                     dimnames=list(NULL,
                                     c("session", "overall","coal","opp"))))
for (i in 1:10) {
  dens_0.3 <- edge_density(fin.graph_0.3[[i]])
  dens_coal <- edge_density(only_coalition_0.3[[i]])
  dens_opp <- edge_density(only_opposition_0.3[[i]])
  dens_thresh_0.3$session[[i]] <- i
  dens_thresh_0.3$overall[[i]] <- dens_0.3
  dens_thresh_0.3$coal[[i]] <- dens_coal
  dens_thresh_0.3$opp[[i]] <- dens_opp
}

plot(dens_thresh_0.3$session, y = dens_thresh_0.3$overall, cex = 3, xlab = "Сесії", 
      ylab = "Щільність", type = "o", col = "green3", pch = 17, lty = 1, lwd = 3, cex.main = 2, cex.lab = 1.5, cex.axis = 1.3, 
      xlim = c(1,10), ylim = c(0.75,1), main = "Щільність мережі з 1 по 10 сесії за мінімального порогу")
  points(dens_thresh_0.3$session, dens_thresh_0.3$coal, cex = 3, col="red3", pch=19)
  lines(dens_thresh_0.3$session, dens_thresh_0.3$coal, col="red3",lty=1, lwd = 3)
  points(dens_thresh_0.3$session, dens_thresh_0.3$opp, cex = 3, col="blue3", pch=18)
  lines(dens_thresh_0.3$session, dens_thresh_0.3$opp, col="blue3",lty=1, lwd = 3)
  legend(8,1.026,legend=c("Загалом","Лише коаліція","Лише опозиція"), 
         col=c("green3","red3","blue3"), cex = 1.1, pt.cex = 3,
         pch=c(17,19,18),lty=c(1,1,1),lwd = c(3,3,3), ncol=1, bty = "n")
  
##################################################################################
### average degrees for factions, coalition, opposition
  
  
  deg_table_0.3 <- data.frame(matrix(ncol=3,nrow=10, 
                                     dimnames=list(NULL,
                                                   c("session", "coalition","opposition"))))
  for (i in 1:10){
    deg_coal <- grouped_metrics_0.3[[i]][1,3]
    deg_opp <- grouped_metrics_0.3[[i]][2,3]
    deg_table_0.3$session[[i]] <- i
    deg_table_0.3$coalition[[i]] <- deg_coal
    deg_table_0.3$opposition[[i]] <- deg_opp
  }
  
  deg_table_0.5 <- data.frame(matrix(ncol=3,nrow=10, 
                                     dimnames=list(NULL,
                                                   c("session", "coalition","opposition"))))
  for (i in 1:10){
    deg_coal <- grouped_metrics[[i]][1,3]
    deg_opp <- grouped_metrics[[i]][2,3]
    deg_table_0.5$session[[i]] <- i
    deg_table_0.5$coalition[[i]] <- deg_coal
    deg_table_0.5$opposition[[i]] <- deg_opp
  }

#####################################################################################
### visualization
  
plot(deg_table_0.3$session, y = deg_table_0.3$coalition, cex = 2.5, xlab = "Сесії", 
      ylab = "Degree", type = "o", col = "red3", pch = 17, lty = 1, lwd = 3, cex.main = 2, cex.lab = 1.5, cex.axis = 1.3, 
      xlim = c(1,10), ylim = c(0.7,1), main = "Міра центральності за мінімального порогового значення")
  points(deg_table_0.3$session, deg_table_0.3$opposition, cex = 2.5, col="blue3", pch=19)
  lines(deg_table_0.3$session, deg_table_0.3$opposition, col="blue3",lty=1, lwd = 3)
  legend(8,1.026,legend=c("Коаліція","Опозиція"), 
         col=c("red3","blue3"), cex = 1.5, pt.cex = 3,
         pch=c(17,19),lty=c(1,1), lwd = c(3,3), ncol=1, bty = "n")
  
  plot(deg_table_0.5$session, y = deg_table_0.5$coalition, cex = 2.5, xlab = "Сесії", 
       ylab = "Degree", type = "o", col = "red3", pch = 17, lty = 1, lwd = 3, cex.main = 2, cex.lab = 1.5, cex.axis = 1.3, 
       xlim = c(1,10), ylim = c(0,1), main = "Міра центральності за порогового значення 0.5")
  points(deg_table_0.5$session, deg_table_0.5$opposition, cex = 2.5, col="blue3", pch=19)
  lines(deg_table_0.5$session, deg_table_0.5$opposition, col="blue3",lty=1, lwd = 3)
  legend(8,1.026,legend=c("Коаліція","Опозиція"), 
         col=c("red3","blue3"), cex = 1.5, pt.cex = 3,
         pch=c(17,19),lty=c(1,1), lwd = c(3,3), ncol=1, bty = "n")


#############################################################################
### degree destribution between factions, coalition, opposition


library(network)
library(sna)
library(statnet)
library(intergraph)

mixed_matrix_0.3 <- list()
opp_matrix_0.3 <- list()
mixingmatrix(asNetwork(fin.graph_0.3[[1]]), "faction")$matrix
for (i in 1:10) {
  mix0.3 <- mixingmatrix(asNetwork(fin.graph_0.3[[i]]), "f.ukr")$matrix
  opp0.3 <- mixingmatrix(asNetwork(fin.graph_0.3[[i]]), "coalition")$matrix
  mixed_matrix_0.3[[i]] <- as.data.table(mix0.3)
  opp_matrix_0.3[[i]] <- as.data.table(opp0.3)
}

tabled_mix_0.3 <- data.frame(matrix(ncol=9,nrow=9, 
                                    dimnames=list(c("Батьківщина","Блок Петра Порошенка",
                                                    "Відродження", "Воля народу", "Народний фронт",
                                                    "Опозиційний блок", "позафракційні", "РП Олега Ляшка",
                                                    "Самопоміч"),
                                                  c("Батьківщина","Блок Петра Порошенка",
                                                    "Відродження", "Воля народу", "Народний фронт",
                                                    "Опозиційний блок", "позафракційні", "РП Олега Ляшка",
                                                    "Самопоміч"))))
tabled_opp_0.3 <- data.frame(matrix(ncol=2,nrow=2, 
                                     dimnames=list(c("Коаліція", "Опозиція"),
                                                   c("Коаліція", "Опозиція"))))



mix_list_0.3 <- list()
opp_list_0.3 <- list()

for (i in 1:10) {
  
  tabled_mix_0.3$Батьківщина <-  mixed_matrix_0.3[[i]][1:9,3]
  tabled_mix_0.3$Блок.Петра.Порошенка <-  mixed_matrix_0.3[[i]][10:18,3]                                                                                                
  tabled_mix_0.3$Відродження <-  mixed_matrix_0.3[[i]][19:27,3]                                                                                                
  tabled_mix_0.3$Воля.народу <-  mixed_matrix_0.3[[i]][28:36,3]                                                                                                
  tabled_mix_0.3$Народний.фронт <-  mixed_matrix_0.3[[i]][37:45,3]                                                                                                
  tabled_mix_0.3$Опозиційний.блок <-  mixed_matrix_0.3[[i]][46:54,3]                                                                                                
  tabled_mix_0.3$позафракційні <-  mixed_matrix_0.3[[i]][55:63,3]                                                                                                
  tabled_mix_0.3$РП.Олега.Ляшка <-  mixed_matrix_0.3[[i]][64:72,3]                                                                                                
  tabled_mix_0.3$Самопоміч <-  mixed_matrix_0.3[[i]][73:81,3] 
  
  tabled_opp_0.3$Коаліція <- opp_matrix_0.3[[i]][c(1,3),3]
  tabled_opp_0.3$Опозиція <- opp_matrix_0.3[[i]][c(2,4),3]

  mix_list_0.3[[i]] <- tabled_mix_0.3
  opp_list_0.3[[i]] <- tabled_opp_0.3
}

#############################################################################
### faction loses through session

faction_losses <- data.frame(matrix(ncol=3,nrow=9, 
                                     dimnames=list(c("Батьківщина","Блок Петра Порошенка",
                                                     "Відродження", "Воля народу", "Народний фронт",
                                                     "Опозиційний блок", "позафракційні", "РП Олега Ляшка",
                                                     "Самопоміч"),
                                                   c("income_val", "outcome_0.3",
                                                     "losses_part_0.3"))))
coalition_losses <- data.frame(matrix(ncol=3,nrow=2, 
                                   dimnames=list(c("Коаліція","Опозиція"),
                                                 c("income_val", "outcome_0.3",
                                                   "losses_part_0.3"))))

flosses_list <- list()
closses_list <- list()

for (i in 1:10) {
  faction_losses$income_val <- c(table(mps_convoc8$f.ukr))
  faction_losses$outcome_0.3 <- c(table(V(fin.graph_0.3[[i]])$f.ukr))
  faction_losses$losses_part_0.3 <- 1-(c(table(V(fin.graph_0.3[[i]])$f.ukr))/c(table(mps_convoc8$f.ukr)))

  coalition_losses$income_val <- c(table(mps_convoc8$coalition))
  coalition_losses$outcome_0.3 <- c(table(V(fin.graph_0.3[[i]])$coalition))
  coalition_losses$losses_part_0.3 <- 1-(c(table(V(fin.graph_0.3[[i]])$coalition))/c(table(mps_convoc8$coalition)))

  flosses_list[[i]] <- faction_losses
  closses_list[[i]] <- coalition_losses}

#############################################################################
### visualization

for(i in 1:10){
  jpeg(paste("maj_graph", i, ".jpg", sep=""), width = 1920,height = 1080,quality = 100)
  plot(fin.graph[[i]], layout=layout_with_fr)
  title(paste("Голосування по сесії ", i,sep = ""),sub = "threshold >0.5",
        cex.main = 2.5, col.main = "black",cex.sub = 2,col.sub = "red")
  legend(x = "left",legend = leg.txt,col = colrs,pch = 20,
         bty = "n",cex = 2,pt.cex = 6,ncol = 1,text.col = "black")
  dev.off()
}
  
#######################################################################################
### graphs from session to session

#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.2369  0.4271  0.4780  0.4599  0.5112  0.5560

fin.graph <- list()
metrics_table <- list()
grouped_metrics <- list()
only_coalition <- list()
only_opposition <- list()
dens_list <- list()

for(i in 1:10){
  fin.m <- sims_per_s[[i]]
  
  fin.m[fin.m<0.5] <- 0
  fin.m[fin.m>0.5] <- 1
  
  fin.g <- graph_from_adjacency_matrix(fin.m, mode = c("undirected"))
  fin.g <- igraph::simplify(fin.g, remove.multiple=FALSE, remove.loops=TRUE)
  igraph::vertex.attributes(fin.g)$faction<-as.character(mps_convoc8$faction)
  igraph::vertex.attributes(fin.g)$ID<-mps_convoc8$ID
  igraph::vertex.attributes(fin.g)$name_ukr<-as.character(mps_convoc8$name_ukr)
  igraph::vertex.attributes(fin.g)$coalition<-mps_convoc8$coalition
  igraph::vertex.attributes(fin.g)$f.ukr<-mps_convoc8$f.ukr
  V(fin.g)$label <- NA
  V(fin.g)$size <- 5
  E(fin.g)$width <- 1
  V(fin.g)[f.ukr=="Батьківщина"]$color <- "darkmagenta"
  V(fin.g)[f.ukr=="Народний фронт"]$color <- "orange"
  V(fin.g)[f.ukr=="Блок Петра Порошенка"]$color <- "darkred"
  V(fin.g)[f.ukr=="РП Олега Ляшка"]$color <- "hotpink"
  V(fin.g)[f.ukr=="Самопоміч"]$color <- "green2"
  V(fin.g)[f.ukr=="Опозиційний блок"]$color <- "blue"
  V(fin.g)[f.ukr=="Воля народу"]$color <- "lightcyan4"
  V(fin.g)[f.ukr=="Відродження"]$color <- "chocolate4"
  V(fin.g)[f.ukr=="позафракційні"]$color <- "gray0"
  V(fin.g)$shape <- "circle"
  fin.g <- igraph::delete.vertices(fin.g, V(fin.g)[ igraph::degree(fin.g)==0 ])
  
  igraph::vertex.attributes(fin.g)$degree<-igraph::degree(fin.g)
  igraph::vertex.attributes(fin.g)$degree_norm<-igraph::degree(fin.g,normalized = T)
  
  table<-data.frame( N = c(table(vertex.attributes(fin.g)$f.ukr)), 
                     degree=tapply(vertex.attributes(fin.g)$degree,vertex.attributes(fin.g)$f.ukr,mean),
                     degree_norm=tapply(vertex.attributes(fin.g)$degree_norm,vertex.attributes(fin.g)$f.ukr,mean))
  table1<-data.frame( N = c(table(vertex.attributes(fin.g)$coalition)),
                      degree=tapply(vertex.attributes(fin.g)$degree,vertex.attributes(fin.g)$coalition,mean),
                      degree_norm=tapply(vertex.attributes(fin.g)$degree_norm,vertex.attributes(fin.g)$coalition,mean))
  metrics_table[[i]] <- table
  grouped_metrics[[i]] <- table1
  
  only_coal <- fin.g
  only_opp <- fin.g
  only_coal <-igraph::delete.vertices(only_coal, V(only_coal)[V(only_coal)$coalition=="opposition"])
  only_opp <-igraph::delete.vertices(only_opp, V(only_opp)[V(only_opp)$coalition=="coalition"])
  
  density_table <- data.frame( sesion = i, 
                               all_edges = gsize(fin.g), coal_edges = gsize(only_coal), 
                               opp_edges = gsize(only_opp), delta_edges = gsize(fin.g) - gsize(only_coal) - gsize(only_opp),
                               N_only_col = sum(table(vertex.attributes(only_coal)$f.ukr)),
                               overall_dense = edge_density(fin.g),
                               coal_dense = edge_density(only_coal), opp_dense = edge_density(only_opp),
                               laws_count = dim(final_graphs[[i]])[2]
  )
  
  
  only_coalition[[i]] <- only_coal
  only_opposition[[i]] <- only_opp
  dens_list[[i]] <- density_table
  fin.graph[[i]] <- fin.g
}


#################################################################################
### denstity and degree destribution through sessions


dens_dens <- do.call(rbind,dens_list)

dens_thresh <- data.frame(matrix(ncol=4,nrow=10, 
                                     dimnames=list(NULL,
                                                   c("session", "overall","coal","opp"))))
for (i in 1:10) {
  dens_0.5 <- edge_density(fin.graph[[i]])
  dens_coal_0.5 <- edge_density(only_coalition[[i]])
  dens_opp_0.5 <- edge_density(only_opposition[[i]])
  dens_thresh$session[[i]] <- i
  dens_thresh$overall[[i]] <- dens_0.5
  dens_thresh$coal[[i]] <- dens_coal_0.5
  dens_thresh$opp[[i]] <- dens_opp_0.5
}

plot(dens_thresh$session, y = dens_thresh$overall, cex = 3, xlab = "Сесії", 
     ylab = "Щільність", type = "o", col = "green3", pch = 17, lty = 1, lwd = 3, cex.main = 2, cex.lab = 1.5, cex.axis = 1.3, 
     xlim = c(1,10), ylim = c(0.1,1), main = "Щільність мережі з 1 по 10 сесії за порогу 0.5")
points(dens_thresh$session, dens_thresh$coal, cex = 3, col="red3", pch=19)
lines(dens_thresh$session, dens_thresh$coal, col="red3",lty=1, lwd = 3)
points(dens_thresh$session, dens_thresh$opp, cex = 3, col="blue3", pch=18)
lines(dens_thresh$session, dens_thresh$opp, col="blue3",lty=1, lwd = 3)
legend(8,1.026,legend=c("Загалом","Лише коаліція","Лише опозиція"), 
       col=c("green3","red3","blue3"), cex = 1.1, pt.cex = 3,
       pch=c(17,19,18),lty=c(1,1,1),lwd = c(3,3,3), ncol=1, bty = "n")



dens_thresh <- data.frame(matrix(ncol=3,nrow=10, 
                                 dimnames=list(NULL,
                                               c("session", "overall","coalition","opposition"))))
for (i in 1:10) {
  dens_0.3 <- edge_density(fin.graph_0.3[[i]])
  dens_thresh$session[[i]] <- i
  dens_thresh$threshold.0.5[[i]] <- dens_0.3
}



#####################################################################################
### degree destribution between factions, coalition, opposition


library(network)
library(sna)
library(statnet)
library(intergraph)

mixed_matrix <- list()
opp_matrix <- list()
mixingmatrix(asNetwork(fin.graph[[1]]), "faction")$matrix
for (i in 1:10) {
  mix <- mixingmatrix(asNetwork(fin.graph[[i]]), "f.ukr")$matrix
  opp <- mixingmatrix(asNetwork(fin.graph[[i]]), "coalition")$matrix
  mixed_matrix[[i]] <- as.data.table(mix)
  opp_matrix[[i]] <- as.data.table(opp)
}

tabled_mix <- data.frame(matrix(ncol=9,nrow=9, 
                                    dimnames=list(c("Батьківщина","Блок Петра Порошенка",
                                                    "Відродження", "Воля народу", "Народний фронт",
                                                    "Опозиційний блок", "позафракційні", "РП Олега Ляшка",
                                                    "Самопоміч"),
                                                  c("Батьківщина","Блок Петра Порошенка",
                                                    "Відродження", "Воля народу", "Народний фронт",
                                                    "Опозиційний блок", "позафракційні", "РП Олега Ляшка",
                                                    "Самопоміч"))))
tabled_opp <- data.frame(matrix(ncol=2,nrow=2, 
                                    dimnames=list(c("Коаліція", "Опозиція"),
                                                  c("Коаліція", "Опозиція"))))



mix_list <- list()
opp_list <- list()

for (i in 1:10) {
  
  tabled_mix$Батьківщина <-  mixed_matrix[[i]][1:9,3]
  tabled_mix$Блок.Петра.Порошенка <-  mixed_matrix[[i]][10:18,3]                                                                                                
  tabled_mix$Відродження <-  mixed_matrix[[i]][19:27,3]                                                                                                
  tabled_mix$Воля.народу <-  mixed_matrix[[i]][28:36,3]                                                                                                
  tabled_mix$Народний.фронт <-  mixed_matrix[[i]][37:45,3]                                                                                                
  tabled_mix$Опозиційний.блок <-  mixed_matrix[[i]][46:54,3]                                                                                                
  tabled_mix$позафракційні <-  mixed_matrix[[i]][55:63,3]                                                                                                
  tabled_mix$РП.Олега.Ляшка <-  mixed_matrix[[i]][64:72,3]                                                                                                
  tabled_mix$Самопоміч <-  mixed_matrix[[i]][73:81,3] 
  
  tabled_opp$Коаліція <- opp_matrix[[i]][c(1,3),3]
  tabled_opp$Опозиція <- opp_matrix[[i]][c(2,4),3]
  
  mix_list[[i]] <- tabled_mix
  opp_list[[i]] <- tabled_opp
}
########################################################################
### faction loses with minimal threshold


faction_losses <- data.frame(matrix(ncol=3,nrow=9, 
                                    dimnames=list(c("Батьківщина","Блок Петра Порошенка",
                                                    "Відродження", "Воля народу", "Народний фронт",
                                                    "Опозиційний блок", "позафракційні", "РП Олега Ляшка",
                                                    "Самопоміч"),
                                                  c("income_val", "outcome_0.3",
                                                    "losses_part_0.3"))))
coalition_losses <- data.frame(matrix(ncol=3,nrow=2, 
                                      dimnames=list(c("Коаліція","Опозиція"),
                                                    c("income_val", "outcome_0.3",
                                                      "losses_part_0.3"))))

flosses_list <- list()
closses_list <- list()

for (i in 1:10) {
  faction_losses$income_val <- c(table(mps_convoc8$f.ukr))
  faction_losses$outcome_0.3 <- c(table(V(fin.graph_0.3[[i]])$f.ukr))
  faction_losses$losses_part_0.3 <- 1-(c(table(V(fin.graph_0.3[[i]])$f.ukr))/c(table(mps_convoc8$f.ukr)))
  
  coalition_losses$income_val <- c(table(mps_convoc8$coalition))
  coalition_losses$outcome_0.3 <- c(table(V(fin.graph_0.3[[i]])$coalition))
  coalition_losses$losses_part_0.3 <- 1-(c(table(V(fin.graph_0.3[[i]])$coalition))/c(table(mps_convoc8$coalition)))
  
  flosses_list[[i]] <- faction_losses
  closses_list[[i]] <- coalition_losses}

###################################################################################
### visualization


for(i in 1:10){
  jpeg(paste("8s_maj", i, ".jpg", sep=""), width = 1920,height = 1080,quality = 100)
  plot(fin.graph[[i]], layout=layout_with_fr)
  title(paste("Голосування по сесії ", i,sep = ""),sub = "threshold >0.5",
        cex.main = 2.5, col.main = "black",cex.sub = 2,col.sub = "red")
  legend(x = "left",legend = leg.txt,col = colrs,pch = 20,
         bty = "n",cex = 2,pt.cex = 6,ncol = 1,text.col = "black")
  dev.off()
}

{
  jpeg(filename = "bind.jpg", width = 1920,height = 1080,quality = 100)
  plot(bind, layout = layout_with_fr)
  title("Голосування з 1 по 10 сесію за мінімального порогу",sub = "threshold >0.2369",
        cex.main = 2.5, col.main = "black",cex.sub = 2,col.sub = "red")
  legend(x = "left",legend = leg.txt,col = colrs,pch = 20,
         bty = "n",cex = 2,pt.cex = 5,ncol = 1,text.col = "black")
  dev.off()
}






##################################################################################################
### BIND ALL SESSIONS 
allses <- do.call(cbind,final_graphs)
allsesok <- do.call(cbind,final_graphs_ok)
dim(allses)[2]
head(allses)
summary(allses)
allses[1:5,1:5]
dim(allses)

allses_count <- list()
for(k in 1:5128){
allses_count[[k]] <- outer(allses[,k], allses[,k], FUN = "==")
}
length(allses_count)

sims_allses <- Reduce('+', allses_count)
dim(sims_allses)
sims_allses_weight <- sims_allses/5128
summary(sims_allses_weight, na.rm=T)

m.allses <- sims_allses_weight

#################################################################################
### average similarity vectors


sim <- apply(sims_allses_weight, 1, mean, na.rm=T)
summary(sim[complete.cases(sim)])
sd(sim[complete.cases(sim)])
mean = 0.3625; sd = 0.04303373;
lb = 0; ub = 1; 
x <- seq(-4,4,length=100)*sd + mean
hx <- dnorm(x,mean,sd)
plot(x, hx, type="n", xlab="Кореляція схожості", ylab="",
     main="Розподіл значень", axes=FALSE, cex.main=2.5, cex.lab=2.5, cex.axis=5)

i <- x >= lb & x <= ub
lines(x, hx)
polygon(c(lb,x[i],ub), c(0,hx[i],0), col="red")

area <- pnorm(ub, mean, sd) - pnorm(lb, mean, sd)
axis(1, at=seq(0, 1, 0.1), pos=0)


#####################################################################################
### all sessions for minimal threshold

#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.2369  0.4271  0.4780  0.4599  0.5112  0.5560

m.allses[m.allses<0.2369] <- 0
m.allses[m.allses>0.2369] <- 1

{bind <- igraph::graph_from_adjacency_matrix(m.allses, mode = c("undirected"))
bind <- igraph::simplify(bind, remove.multiple=FALSE, remove.loops=TRUE)
igraph::vertex.attributes(bind)$faction<-as.character(mps_convoc8$faction)
igraph::vertex.attributes(bind)$ID<-mps_convoc8$ID
igraph::vertex.attributes(bind)$name_ukr<-as.character(mps_convoc8$name_ukr)
igraph::vertex.attributes(bind)$coalition<-mps_convoc8$coalition
igraph::vertex.attributes(bind)$f.ukr<-mps_convoc8$f.ukr
V(bind)$label <- NA
V(bind)$size <- 5
E(bind)$width <- 1
V(bind)[f.ukr=="Батьківщина"]$color <- "darkmagenta"
V(bind)[f.ukr=="Народний фронт"]$color <- "orange"
V(bind)[f.ukr=="Блок Петра Порошенка"]$color <- "darkred"
V(bind)[f.ukr=="РП Олега Ляшка"]$color <- "hotpink"
V(bind)[f.ukr=="Самопоміч"]$color <- "green2"
V(bind)[f.ukr=="Опозиційний блок"]$color <- "blue"
V(bind)[f.ukr=="Воля народу"]$color <- "lightcyan4"
V(bind)[f.ukr=="Відродження"]$color <- "chocolate4"
V(bind)[f.ukr=="позафракційні"]$color <- "gray0"
V(bind)$shape <- "circle"
bind <-igraph::delete.vertices(bind, V(bind)[ igraph::degree(bind)==0 ])

only_coal <- bind
only_opp <- bind
only_coal <-igraph::delete.vertices(only_coal, V(only_coal)[V(only_coal)$coalition=="opposition"])
only_opp <-igraph::delete.vertices(only_opp, V(only_opp)[V(only_opp)$coalition=="coalition"])

density_table <- data.frame( sesion = "сесія з 1 по 10", 
                             all_edges = gsize(bind), coal_edges = gsize(only_coal), 
                             opp_edges = gsize(only_opp), delta_edges = gsize(bind) - gsize(only_coal) - gsize(only_opp),
                             N_only_col = sum(table(vertex.attributes(only_coal)$f.ukr)),
                             overall_dense = edge_density(bind),
                             maj_dense = edge_density(only_coal),opp_dense = edge_density(only_opp),
                             laws_count = dim(allses)[2])




igraph::vertex.attributes(bind)$clus_l<-igraph::transitivity(bind, type = "local",isolates = "zero")
igraph::vertex.attributes(bind)$degree<-igraph::degree(bind)
igraph::vertex.attributes(bind)$degree_norm<-igraph::degree(bind,normalized = T)
igraph::vertex.attributes(bind)$between<-igraph::betweenness(bind)
igraph::vertex.attributes(bind)$between_norm<-igraph::betweenness(bind,normalized = T)
igraph::vertex.attributes(bind)$closen<-igraph::closeness(bind)
igraph::vertex.attributes(bind)$closen_norm<-igraph::closeness(bind, normalized = T)
igraph::vertex.attributes(bind)$eigen<-igraph::eigen_centrality(bind)$vector}



################################################################################
### all sessions for threshold 0.5

m.allses_0.5 <- sims_allses_weight

#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.2369  0.4271  0.4780  0.4599  0.5112  0.5560

m.allses_0.5[m.allses_0.5<0.5] <- 0
m.allses_0.5[m.allses_0.5>0.5] <- 1

{bind_0.5 <- igraph::graph_from_adjacency_matrix(m.allses_0.5, mode = c("undirected"))
  bind_0.5 <- igraph::simplify(bind_0.5, remove.multiple=FALSE, remove.loops=TRUE)
  igraph::vertex.attributes(bind_0.5)$faction<-as.character(mps_convoc8$faction)
  igraph::vertex.attributes(bind_0.5)$ID<-mps_convoc8$ID
  igraph::vertex.attributes(bind_0.5)$name_ukr<-as.character(mps_convoc8$name_ukr)
  igraph::vertex.attributes(bind_0.5)$coalition<-mps_convoc8$coalition
  igraph::vertex.attributes(bind_0.5)$f.ukr<-mps_convoc8$f.ukr
  V(bind_0.5)$label <- NA
  V(bind_0.5)$size <- 5
  E(bind_0.5)$width <- 1
  V(bind_0.5)[f.ukr=="Батьківщина"]$color <- "darkmagenta"
  V(bind_0.5)[f.ukr=="Народний фронт"]$color <- "orange"
  V(bind_0.5)[f.ukr=="Блок Петра Порошенка"]$color <- "darkred"
  V(bind_0.5)[f.ukr=="РП Олега Ляшка"]$color <- "hotpink"
  V(bind_0.5)[f.ukr=="Самопоміч"]$color <- "green2"
  V(bind_0.5)[f.ukr=="Опозиційний блок"]$color <- "blue"
  V(bind_0.5)[f.ukr=="Воля народу"]$color <- "lightcyan4"
  V(bind_0.5)[f.ukr=="Відродження"]$color <- "chocolate4"
  V(bind_0.5)[f.ukr=="позафракційні"]$color <- "gray0"
  V(bind_0.5)$shape <- "circle"
  bind_0.5 <-igraph::delete.vertices(bind_0.5, V(bind_0.5)[ igraph::degree(bind_0.5)==0 ])
  
  only_coal <- bind_0.5
  only_opp <- bind_0.5
  only_coal <-igraph::delete.vertices(only_coal, V(only_coal)[V(only_coal)$coalition=="opposition"])
  only_opp <-igraph::delete.vertices(only_opp, V(only_opp)[V(only_opp)$coalition=="coalition"])
  
  density_table_0.5 <- data.frame( sesion = "сесія з 1 по 10", 
                               all_edges = gsize(bind_0.5), coal_edges = gsize(only_coal), 
                               opp_edges = gsize(only_opp), delta_edges = gsize(bind_0.5) - gsize(only_coal) - gsize(only_opp),
                               N_only_col = sum(table(vertex.attributes(only_coal)$f.ukr)),
                               overall_dense = edge_density(bind_0.5),
                               maj_dense = edge_density(only_coal), opp_dense = edge_density(only_opp),
                               laws_count = dim(allses)[2])
  
  
  
  
  igraph::vertex.attributes(bind_0.5)$clus_l<-igraph::transitivity(bind_0.5, type = "local",isolates = "zero")
  igraph::vertex.attributes(bind_0.5)$degree<-igraph::degree(bind_0.5)
  igraph::vertex.attributes(bind_0.5)$degree_norm<-igraph::degree(bind_0.5,normalized = T)
  igraph::vertex.attributes(bind_0.5)$between<-igraph::betweenness(bind_0.5)
  igraph::vertex.attributes(bind_0.5)$between_norm<-igraph::betweenness(bind_0.5,normalized = T)
  igraph::vertex.attributes(bind_0.5)$closen<-igraph::closeness(bind_0.5)
  igraph::vertex.attributes(bind_0.5)$closen_norm<-igraph::closeness(bind_0.5, normalized = T)
  igraph::vertex.attributes(bind_0.5)$eigen<-igraph::eigen_centrality(bind_0.5)$vector}



######################################################################################
### average degree per faction, coalition, opposition

table_bind<-data.frame( N = c(table(vertex.attributes(bind)$f.ukr)),
                       degree=tapply(vertex.attributes(bind)$degree,vertex.attributes(bind)$f.ukr,mean),
                       degree_norm=tapply(vertex.attributes(bind)$degree_norm,vertex.attributes(bind)$f.ukr,mean))

table_bind_opp<-data.frame( N = c(table(vertex.attributes(bind)$coalition)),
                    degree=tapply(vertex.attributes(bind)$degree,vertex.attributes(bind)$coalition,mean),
                    degree_norm=tapply(vertex.attributes(bind)$degree_norm,vertex.attributes(bind)$coalition,mean))


table_bind_0.5<-data.frame( N = c(table(vertex.attributes(bind_0.5)$f.ukr)),
                        degree=tapply(vertex.attributes(bind_0.5)$degree,vertex.attributes(bind_0.5)$f.ukr,mean),
                        degree_norm=tapply(vertex.attributes(bind_0.5)$degree_norm,vertex.attributes(bind_0.5)$f.ukr,mean))

table_bind_opp_0.5<-data.frame( N = c(table(vertex.attributes(bind_0.5)$coalition)),
                            degree=tapply(vertex.attributes(bind_0.5)$degree,vertex.attributes(bind_0.5)$coalition,mean),
                            degree_norm=tapply(vertex.attributes(bind_0.5)$degree_norm,vertex.attributes(bind_0.5)$coalition,mean))
########################################################
### faction loses 

bf_losses <- data.frame(matrix(ncol=5,nrow=9, 
                                    dimnames=list(c("Батьківщина","Блок Петра Порошенка",
                                                    "Відродження", "Воля народу", "Народний фронт",
                                                    "Опозиційний блок", "позафракційні", "РП Олега Ляшка",
                                                    "Самопоміч"),
                                                  c("income_val", "outcome_0.3", "delta",
                                                    "losses_abs", "losses_rel"))))
bc_losses <- data.frame(matrix(ncol=5,nrow=2, 
                                      dimnames=list(c("Коаліція","Опозиція"),
                                                    c("income_val", "outcome_0.3", "delta",
                                                      "losses_abs", "losses_rel"))))



{
  bf_losses$income_val <- c(table(mps_convoc8$f.ukr))
  bf_losses$outcome_0.3 <- c(table(V(bind)$f.ukr))
  bf_losses$delta <- c(table(mps_convoc8$f.ukr)) - c(table(V(bind)$f.ukr))
  bf_losses$losses_abs <- 1-(c(table(V(bind)$f.ukr))/c(table(mps_convoc8$f.ukr)))
  bf_losses$losses_rel <- (c(table(mps_convoc8$f.ukr)) - c(table(V(bind)$f.ukr)))/sum(c(table(mps_convoc8$f.ukr)) - c(table(V(bind)$f.ukr)))
  
  bc_losses$income_val <- c(table(mps_convoc8$coalition))
  bc_losses$outcome_0.3 <- c(table(V(bind)$coalition))
  bc_losses$delta <- c(table(mps_convoc8$coalition)) - c(table(V(bind)$coalition))
  bc_losses$losses_abs <- 1-(c(table(V(bind)$coalition))/c(table(mps_convoc8$coalition)))
  bc_losses$losses_rel <- (c(table(mps_convoc8$coalition)) - c(table(V(bind)$coalition)))/sum(c(table(mps_convoc8$coalition)) - c(table(V(bind)$coalition)))}


#############################################################################
#### mixing matrix for degree destribution


bind_mix_0.5 <- data.frame(matrix(ncol=9,nrow=9, 
                                    dimnames=list(c("Батьківщина","Блок Петра Порошенка",
                                                    "Відродження", "Воля народу", "Народний фронт",
                                                    "Опозиційний блок", "позафракційні", "РП Олега Ляшка",
                                                    "Самопоміч"),
                                                  c("Батьківщина","Блок Петра Порошенка",
                                                    "Відродження", "Воля народу", "Народний фронт",
                                                    "Опозиційний блок", "позафракційні", "РП Олега Ляшка",
                                                    "Самопоміч"))))
bind_opp_0.5 <- data.frame(matrix(ncol=2,nrow=2, 
                                    dimnames=list(c("Коаліція", "Опозиція"),
                                                  c("Коаліція", "Опозиція"))))


{
  mix <- as.data.table(mixingmatrix(asNetwork(bind_0.5), "f.ukr")$matrix)
  opp <- as.data.table(mixingmatrix(asNetwork(bind_0.5), "coalition")$matrix)
  
  bind_mix_0.5$Батьківщина <-  mix[1:9,3]
  bind_mix_0.5$Блок.Петра.Порошенка <-   mix[10:18,3]                                                                                               
  bind_mix_0.5$Відродження <-   mix[19:27,3]                                                                                               
  bind_mix_0.5$Воля.народу <-  mix[28:36,3]                                                                                                
  bind_mix_0.5$Народний.фронт <-  mix[37:45,3]                                                                                                
  bind_mix_0.5$Опозиційний.блок <-  mix[46:54,3]                                                                                                
  bind_mix_0.5$позафракційні <-  mix[55:63,3]                                                                                                
  bind_mix_0.5$РП.Олега.Ляшка <-  mix[64:72,3]                                                                                                
  bind_mix_0.5$Самопоміч <- mix[73:81,3] 
  
  bind_opp_0.5$Коаліція <- as.data.table(opp[c(1,3),3])
  bind_opp_0.5$Опозиція <- as.data.table(opp[c(2,4),3])}


tabled_mix_0.5 <- data.frame(matrix(ncol=9,nrow=9, 
                                    dimnames=list(c("Батьківщина","Блок Петра Порошенка",
                                                    "Відродження", "Воля народу", "Народний фронт",
                                                    "Опозиційний блок", "позафракційні", "РП Олега Ляшка",
                                                    "Самопоміч"),
                                                  c("Батьківщина","Блок Петра Порошенка",
                                                    "Відродження", "Воля народу", "Народний фронт",
                                                    "Опозиційний блок", "позафракційні", "РП Олега Ляшка",
                                                    "Самопоміч"))))
tabled_opp_0.5 <- data.frame(matrix(ncol=2,nrow=2, 
                                    dimnames=list(c("Коаліція", "Опозиція"),
                                                  c("Коаліція", "Опозиція"))))



mix_list_0.5 <- list()
opp_list_0.5 <- list()

for (i in 1:10) {
  
  tabled_mix_0.5$Батьківщина <-  bind_mix_0.5[[i]][1:9,3]
  tabled_mix_0.5$Блок.Петра.Порошенка <-  bind_mix_0.5[[i]][10:18,3]                                                                                                
  tabled_mix_0.5$Відродження <-  bind_mix_0.5[[i]][19:27,3]                                                                                                
  tabled_mix_0.5$Воля.народу <-  bind_mix_0.5[[i]][28:36,3]                                                                                                
  tabled_mix_0.5$Народний.фронт <-  bind_mix_0.5[[i]][37:45,3]                                                                                                
  tabled_mix_0.5$Опозиційний.блок <-  bind_mix_0.5[[i]][46:54,3]                                                                                                
  tabled_mix_0.5$позафракційні <-  bind_mix_0.5[[i]][55:63,3]                                                                                                
  tabled_mix_0.5$РП.Олега.Ляшка <-  bind_mix_0.5[[i]][64:72,3]                                                                                                
  tabled_mix_0.5$Самопоміч <-  bind_mix_0.5[[i]][73:81,3] 
  
  tabled_opp_0.5$Коаліція <- bind_opp_0.5[[i]][c(1,3),3]
  tabled_opp_0.5$Опозиція <- bind_opp_0.5[[i]][c(2,4),3]
  
  mix_list_0.5[[i]] <- tabled_mix_0.5
  opp_list_0.5[[i]] <- tabled_opp_0.5
}
#######################################################################################
### overall visualization

{
  jpeg(filename = "bind.jpg", width = 1920,height = 1080,quality = 100)
  plot(bind, layout = layout_with_fr)
  title("Голосування з 1 по 10 сесію за мінімального порогу",sub = "threshold >0.2369",
        cex.main = 2.5, col.main = "black",cex.sub = 2,col.sub = "red")
  legend(x = "left",legend = leg.txt,col = colrs,pch = 20,
         bty = "n",cex = 2,pt.cex = 5,ncol = 1,text.col = "black")
  dev.off()
}



{
  jpeg(filename = "bind_maj.jpg", width = 1920,height = 1080,quality = 100)
  plot(bind_0.5, layout = layout_with_fr)
  title("Голосування з 1 по 10 сесію за умови порогу 0.5",sub = "threshold >0.5",
        cex.main = 2.5, col.main = "black",cex.sub = 2,col.sub = "red")
  legend(x = "left",legend = leg.txt,col = colrs,pch = 20,
         bty = "n",cex = 2,pt.cex = 5,ncol = 1,text.col = "black")
  dev.off()
}


##################################################################################################


