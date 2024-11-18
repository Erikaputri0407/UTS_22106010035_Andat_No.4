#***NOMOR 2***
#READ YOUR DATA
data = read.csv("Analisis.csv", header = TRUE, sep = ",")
data
#CHECK THE PACKACING
nrow(data)
ncol(data)
str(data)
#LOOK AT THE TOP AND THE BOTTOM OF YOUR DATA
head(data$TimeSpentOnCourse)
head(data$QuizScores)
tail(data$TimeSpentOnCourse)
tail(data$QuizScores)
#CHECKING YOUR "n"s
head(table(data$TimeSpentOnCourse))
head(table(data$QuizScores))
install.packages("dplyr")
library(dplyr)
select(data, TimeSpentOnCourse, QuizScores, CourseCompletion)
#VALIDATE WITH AT LEAST ONE EXTERNAL DATA SOURCE
summary(data$TimeSpentOnCourse)
summary(data$QuizScores)
quantile(data$TimeSpentOnCourse)
quantile(data$QuizScores)
#MAKE A PLOT
library(ggplot2)
ggplot(data,aes(x=TimeSpentOnCourse, y=CourseCompletion)) + geom_point() + labs(title="Durasi Belajar vs Tingkat penyelesaian kursus", x= "Durasi Belajar", y="Tingkat penyelesaian Kursus")
ggplot(data,aes(x=QuizScores, y=CourseCompletion)) + geom_point() + labs(title="Skor Quiz vs Tingkat penyelesaian kursus", x= "Skor Quiz", y="Tingkat penyelesaian Kursus")
ggplot(data, aes(x=CourseCompletion)) + geom_histogram() + labs(title="Distribusi Tingkat Penyelesaian Kursus", x="Tingkat penyelesaian kursus", y="Frekuensi")
#TRY THE EASY SOLUTION FIRST
cor(data$TimeSpentOnCourse, data$CourseCompletion)
cor(data$QuizScores, data$CourseCompletion)
#FOLLOW UP QUESTION
#1. Apakah anda memiliki data yang benar?
#Ya, Data diatas menunjukan bahwa variabel independent adalah durasi belajar dan skor quiz, sedangkan untuk variabel dependennya adalah tingkat penyelesaian kursus
#kumpulan data ini dapat menjawab apakah dua independent tersebut mempengaruhi tingkat penyelesaian kursus
#3). Apakah pertanyaan anda tepat?
#Mungkin pertanyaan tersebut dapat dijadikan sebuah pertanyaan yang lebih menarik seperti "Apakah durasi waktu belajar dan skor quiz dapat berpengaruh sangat signifikan terhadap tingkat penyelesaian kursus online?

#***NOMOR 3***
library(ggplot2)
#COMPARING MODEL EKSPECTATIONS TO REALITY : BUATLAH HISTOGRAM VARIABEL DEPENDENT DAN BANDINGKAN DENGAN HISTOGRAM DATA YANG BERDISTRIBUSI NORMAL
#HISTOGRAM DEPENDENT
ggplot(data, aes(x=CourseCompletion),binwidth=0.05)+geom_histogram()+labs("title = Histogram Tingkat penyelesaian kursus")
#HISTOGRAM BERDISTRIBUSI NORMAL
mean_data = mean(data$CourseCompletion, na.rm=TRUE)
sd_data = sd(data$CourseCompletion, na.rm = TRUE)
ggplot(data, aes(x=CourseCompletion), binwidth=0.05) + geom_histogram()+ stat_function(fun=dnorm, args = list(mean = mean_data, sd = sd_data)) + labs("Histogram Tingkat Penyelesaian Kursus Berdistribusi normal")
#UJI NORMALIRAS
tes_shapiro = shapiro.test(data$CourseCompletion)
tes_shapiro

model = lm(CourseCompletion~TimeSpentOnCourse+QuizScores, data = data)
summary(model)                                                                        
