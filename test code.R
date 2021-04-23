dat <- read.csv('gap.csv')

gdp <- dat[,3:14]
years <- seq(1952, 2007,5)
colnames(gdp) <- years
rownames(gdp) <- dat[,2]

lifeExp <- dat[,15:26]
colnames(lifeExp) <- years
rownames(lifeExp) <- dat[,2]






1
# Some basic exploratory data analysis plots, showing how GDP and life expectancy have changed over
# the past 70 years. 

# In particular, you should calculate the average life expectancy and GDP per capita
# for each continent through time and plot these.

#改成常用数据表

gdp_temp <- data.frame()
gdp_temp <- data.frame(gdpPercap=unlist(gdp))
gdp_temp$year <- as.integer(substring(rownames(gdp_temp),1,4))
gdp_temp$continent <- dat[,1]
gdp_temp$country <- dat[,2]
rownames(gdp_temp) <- NULL

gdp_struc <- gdp_temp

lifeExp_temp <- data.frame()
lifeExp_temp <- data.frame(lifeExp=unlist(lifeExp))
lifeExp_temp$year <- as.integer(substring(rownames(lifeExp_temp),1,4))
lifeExp_temp$continent <- dat[,1]
lifeExp_temp$country <- dat[,2]
rownames(lifeExp_temp) <- NULL

lifeExp_struc <- lifeExp_temp

#汇总gdp
gdp_struc_mean <- 
        gdp_struc %>%
        group_by(continent,year) %>%
        summarise(n=n()
                  ,gdpPercap_mean = mean(gdpPercap))
#画图gdp
g <- ggplot(gdp_struc_mean, aes(year, gdpPercap_mean, color = continent))
g +
        geom_point() +
        geom_line() +
        theme_bw()
#汇总lifeExp
lifeExp_struc_mean <- 
        lifeExp_struc %>%
        group_by(continent,year) %>%
        summarise(n=n()
                  ,lifeExp_mean = mean(lifeExp))
#画图lifeExp
g <- ggplot(lifeExp_struc_mean, aes(year, lifeExp_mean, color = continent))
g +
        geom_point() +
        geom_line() +
        theme_bw()
#include other plots of your choice
????????




        
#Carry out principal component analysis

#gdp
#work with the log of the GDP
log_gdp <- log(gdp)
#标准化
log_gdp_scale = scale(log_gdp)
#计算相关系数
log_gdp_cor <- cor(log_gdp_scale)
#计算特征值和特征向量
log_gdp_eigen=eigen(log_gdp_cor)
log_gdp_var=log_gdp_eigen$values ## 相关系数矩阵的特征值
options(digits=4, scipen=4)
log_gdp_var
log_gdp_pca_var=log_gdp_var/sum(log_gdp_var)
log_gdp_pca_var
#画碎石图
g <- ggplot(,aes(x=1:12,y=log_gdp_pca_var))
g+geom_point(lwd=3,col='steelblue')+
        geom_line(col='steelblue',lwd=1.2)+
        labs(x = "Principal Component", y = "Proportion of Variance Explained")+
        theme_bw()


#lifeExp
#标准化
lifeExp_scale = scale(lifeExp)
#计算相关系数
lifeExp_cor <- cor(lifeExp_scale)
#计算特征值和特征向量
lifeExp_eigen=eigen(lifeExp_cor)
lifeExp_var=lifeExp_eigen$values ## 相关系数矩阵的特征值
options(digits=4, scipen=4)
lifeExp_var
lifeExp_pca_var=lifeExp_var/sum(lifeExp_var)
lifeExp_pca_var
#画碎石图
g <- ggplot(,aes(x=1:12,y=lifeExp_pca_var))
g+geom_point(lwd=3,col='steelblue')+
        geom_line(col='steelblue',lwd=1.2)+
        labs(x = "Principal Component", y = "Proportion of Variance Explained")+
        theme_bw()






#scatter plots of combinations of the first three principal component scores
#连接经济年龄数据
combined <- cbind(log(dat[,3:14]), dat[,15:26])
#PCA主成分分析
combined_pca = princomp(combined,cor=T)
#summary(combined_pca)
#提取3个主成分
combined_3D = combined_pca$scores[,1:3]
colnames(combined_3D) <- c('PC1','PC2','PC3')
combined_3D <- data.frame(combined_3D)
#画图标签准备
color <- as.numeric(dat[,1])
continent <- dat[,1]
country <- dat[,2]
#画图
plot3d(combined_3D,col=color,radius=50)
g <- ggplot(combined_3D,aes(x=PC1,y=PC2,color=continent))
g+geom_point(lwd=2)+
        geom_text(aes(label = country),nudge_y = 0.25)+
        theme_bw()







#MDS
#计算距离
combined.dist <- dist(combined, method = "euclidean")
#MDS分析
combined.MDS <- cmdscale(combined.dist, k = 2)
summary(combined.MDS)
#命名列名
colnames(combined.MDS) <- c('V1','V2')
combined.MDS <- data.frame(combined.MDS)
#画图
g <- ggplot(combined.MDS,aes(x=V1,y=V2,color=continent))
g+geom_point(lwd=2)+theme_bw()