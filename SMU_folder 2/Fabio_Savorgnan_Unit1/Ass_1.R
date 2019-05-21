> matrix<-matrix(data = c(2,2,3,2,4,2,2), nrow = 7, ncol = 1, byrow = TRUE,dimnames = NULL)
> matrix
[,1]
[1,]    2
[2,]    2
[3,]    3
[4,]    2
[5,]    4
[6,]    2
[7,]    2
> Fabio<-c("computer programming", "math", "statistics", "machine learning", "domain expertise", "communication and presentation skills", "data visualization")
> Fabio
[1] "computer programming"                 
[2] "math"                                 
[3] "statistics"                           
[4] "machine learning"                     
[5] "domain expertise"                     
[6] "communication and presentation skills"
[7] "data visualization"                   
> matrix2<-cbind(matrix, Fabio)
> matrix2
Fabio                                  
[1,] "2" "computer programming"                 
[2,] "2" "math"                                 
[3,] "3" "statistics"                           
[4,] "2" "machine learning"                     
[5,] "4" "domain expertise"                     
[6,] "2" "communication and presentation skills"
[7,] "2" "data visualization"                   
> matrix2<-cbind(Fabio, matrix)
> matrix2
Fabio                                      
[1,] "computer programming"                  "2"
[2,] "math"                                  "2"
[3,] "statistics"                            "3"
[4,] "machine learning"                      "2"
[5,] "domain expertise"                      "4"
[6,] "communication and presentation skills" "2"
[7,] "data visualization"                    "2"
> df<-data.frame(matrix2)
> df
Fabio V2
1                  computer programming  2
2                                  math  2
3                            statistics  3
4                      machine learning  2
5                      domain expertise  4
6 communication and presentation skills  2
7                    data visualization  2
> colnames(df)<-c("Fabio", "Score")
> df
Fabio Score
1                  computer programming     2
2                                  math     2
3                            statistics     3
4                      machine learning     2
5                      domain expertise     4
6 communication and presentation skills     2
7                    data visualization     2
> Fabio<-df
> Fabio
Fabio Score
1                  computer programming     2
2                                  math     2
3                            statistics     3
4                      machine learning     2
5                      domain expertise     4
6 communication and presentation skills     2
7                    data visualization     2
barplot(height = c(2,2,3,2,4,2,2), width = 1, space = NULL, names.arg = c("computer programming", "math", "statistics", "machine learning", "domain expertise", "communication and presentation skills", "data visualization"), legend.text = "Fabio Profile")
> barplot(height = c(2,2,3,2,4,2,2), width = 1, space = NULL, names.arg = c("CP", "Math", "Stat", "ML", "DE", "Com", "DV"), legend.text = "Fabio Profile")