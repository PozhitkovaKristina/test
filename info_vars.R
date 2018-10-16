source("C:/!Kristina/005_PIL_FA_inactive_base/02_mod_hist_data/libr.R")

flattenCorrMatrix <- function(cormat, pmat, nmat) {
  ut <- upper.tri(cormat)
  data.table(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = round((pmat)[ut],3),
    n = (nmat)[ut]
  )
}

cd<-"C:/!Kristina/Methods/sdsj"
setwd(cd)
cd_data <-sprintf("%s/sdsj2018_automl_check_datasets",cd)
setwd(cd_data)
data_list<-dir(cd_data)

for (i in c(1:length(data_list))) {

  setwd(sprintf("%s/%s", cd_data, data_list[i]))
  
  train       <-read.csv("train.csv")
  test        <-read.csv("test.csv")
  test_target <-read.csv("test-target.csv")

  share_NA_train <-cbind(data.frame("vars"=names(train)), 
                   data.frame("share_na"=colSums(is.na(train))/nrow(train)*100))

  vars_full_missing_train <- levels((share_NA_train  %>% 
                                     filter(share_na==100) %>% 
                                     droplevels)$vars)

  summary_vars_train<-data.frame(summary(train%>%select(-matches("string"), 
                                                        -c(vars_full_missing_train ))))%>%
    select(variables=Var2, Freq) %>%
    separate(Freq, c("stats", "value"), sep=":")%>%
    mutate( value = as.numeric(value) )%>%
    spread(stats, value)%>%
    select(-"<NA>")
 
  
  share_NA_test <-cbind(data.frame("vars"=names(test)), 
                         data.frame("share_na"=colSums(is.na(test))/nrow(test)*100))
  
  vars_full_missing_test <- levels((share_NA_test%>% 
                                       filter(share_na==100) %>% 
                                       droplevels)$vars)
  
  summary_vars_test<-data.frame(summary(test%>%select(-matches("string"), 
                                                        -c(vars_full_missing_test))))%>%
    select(variables=Var2, Freq) %>%
    separate(Freq, c("stats", "value"), sep=":")%>%
    mutate( value = as.numeric(value) )%>%
    spread(stats, value)%>%
    select(-"<NA>")
  
    
    corr_matrix<-rcorr(as.matrix(train %>% select(matches("number"))))
    
    out_corr<-as.data.table(flattenCorrMatrix(
      corr_matrix$r, 
      corr_matrix$P,
      corr_matrix$n))[ order(-abs(cor))]
    
  write.xlsx(list("share_NA_train" = share_NA_train,
                  "vars_full_missing_train" =vars_full_missing_train,
                  "summary_vars_train"=summary_vars_train,
                  "share_NA_test" = share_NA_test,
                  "vars_full_missing_test" =vars_full_missing_test,                 
                  "summary_vars_test"=summary_vars_test,
                  "corr_number_vars" = out_corr), sprintf("out_summary_corr_%s.xlsx",i))

}  
  


names(train)






str(train )


library(corrplot)





varlist <- names(train%>%select(-line_id, -target))
for (i in c(1:vars) { 
i=2
var<-train %>% select(varlist[i])


if 
t<- un(train %>% select(varlist[i]))
table(var)
if 
print(summary(var))
if (typeof(train$target)=='double' {
table(train$target)
  
print(ggplot(var, aes(x=varlist[i]), ) + 
      geom_histogram(aes(y=..density..), alpha=0.5, position="identity")+
      geom_density(alpha=.2))
       
}

ggplot(var, aes(x=varlist[i])) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666") 
# Color by groups
ggplot(var, aes(x=varlist[i], color=as.factor(target, fill=target)) +
  geom_density(alpha=.2) 

  
  + 
    geom_histogram(aes(y=..density..), alpha=0.5, 
                   position="identity")+
