library(data.table)

  set.seed(5)
  n=2000 #number of people
  new_disp=20  # ���������� ���������� �� ��������, ����� ��������� ��������� ���� ���������� �/� ������������
  data<-data.table(base_scale=rnorm(n))
  data[,new_scale:=base_scale+rnorm(n)*new_disp]
  
  View(data)
  
  #������������ ����������
  #cor(data, method='kendall) - default=Pearson, method='spearman' , �� ���� - ��� ������
  #cor.test - �� ���� ��������� 2 ����������, ���� ����������� ����������
  correl<-data[,cor.test(base_scale,new_scale)]
  correl
  typeof(correl)
  
  
  #GGPLOT
  library(ggplot2)
  #aes=aesthetic, ����������� ������ � ��������� �������: �����, �������, ������������ �� x, y - � �.�.
  #geom_point - geom � ���� �����
  geom_area()
  ggplot(data=data,aes(x=base_scale,y=new_scale)) + geom_point()
  ggplot(data=data,aes(x=base_scale,y=new_scale)) + geom_area()
  ggplot(data=data,aes(x=base_scale,y=new_scale)) + geom_bin2d()
  ggplot(data=data,aes(x=base_scale,y=new_scale)) + geom_density_2d()
  ggplot(data=data,aes(x=base_scale,y=new_scale, color=dot_color)) + geom_point(size=0.1)
  ggplot(data=data,aes(x=base_scale,y=new_scale, col=)) + geom_point(size=0.1)+ stat_smooth(method = "lm", col = "red")
  ggplot(data=data,aes(x=base_scale,y=new_scale)) + geom_point(size=0.1)+ stat_smooth(method = "lm", col = "red") + annotate("text",x = -1,y=50,label=correl$estimate)
  ggplot(data=data,aes(x=base_scale,y=new_scale)) + geom_point(size=0.1)+ stat_smooth(method = "lm", col = "red") + annotate("text",x = -1,y=50,label=correl$estimate) + ggtitle('Base scale vs New scale') +theme_bw() +theme(plot.title = element_text(color = 'red', size=10, hjust = 0.5)) 
  
  data[, dot_color:=sample(c('red', 'green', 'blue'), n, T)]
  
  

