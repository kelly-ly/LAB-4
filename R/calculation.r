linreg<-setRefClass("linreg",fields=list(formula='formula',data='data.frame',reg_coe='matrix',fitted_value='matrix',residuals='matrix',df='numeric',res_var='matrix',var_reg_coe='matrix',t_value='matrix'),methods=list(
  initialize = function(formula,data){
    formula<<-formula
    data<<-data
    calculate_Regressions_coefficients()
    calculate_the_fitted_value()
    calculate_the_residuals()
    calculate_degress_of_freedom()
    calculate_residual_variance()
    calculate_var_reg_coe()
    calculate_t_value()
  },
  calculate_Regressions_coefficients=function(){
    x<-get_matrix_x()
    y<-get_matrix_y()
    reg_coe<<-solve((t(x)%*%x))%*%t(x)%*%y
  },
  calculate_the_fitted_value=function(){
    fitted_value<<-get_matrix_x()%*%reg_coe
  },
  calculate_the_residuals=function(){
    residuals<<-get_matrix_y()-fitted_value
  },
  get_matrix_y=function(){
    #formula<-formula(formula)
    y_name<-all.vars(formula)[1]
    y<-data[y_name]
    return (as.matrix(y))
  },
  calculate_degress_of_freedom=function(){
    x<-get_matrix_x()
    df<<-nrow(x)-ncol(x)
  },
  get_matrix_x=function(){
    fm<-formula(formula)
    fm<-lm(fm,data)
    x<-model.matrix(fm)
    # delete intercept
    return (as.matrix(x))
    #print(x[,-1])
  },
  calculate_residual_variance=function(){
    #return(print(df))
    res_var<<-(t(residuals)%*%residuals)/df
  },
  calculate_var_reg_coe=function(){
    x<-get_matrix_x()
    var_reg_coe<<-res_var[1,1]*solve((t(x)%*%x))
  },
  calculate_t_value=function(){
    t_value<<-reg_coe/sqrt(diag(var_reg_coe))
    #t_value<-reg_coe/sqrt(var(reg_coe))
    #return(t_value)
    #return(sqrt(var(reg_coe)))
  },
  # print=function(){
  #   str2<-cat('Coefficients:\n',reg_coe[,1])
  #   #v<-c(str2,reg_coe[,1])
  #   return(str2)
  # },
  plot=function(){
    d<-c()
    for(i in 1:nlevels(factor(fitted_value))){
      pos<-which(fitted_value==levels(factor(fitted_value))[i])
      #m<-mean(residuals[pos[1]:pos[length(pos)]])
      m<-median(residuals[pos[1]:pos[length(pos)]])
      #m<-mean(l1$residuals(pos[1]:pos[length(pos)]))
      d<-c(d,m)
    }
    
    axe_x<-as.numeric(levels(factor(fitted_value)))
    df1<-data.frame(fv=fitted_value,rs=residuals)
    names(df1)<-c("fv","rs")
    df2<-data.frame(fv=axe_x,rs=d)
  
    plot1<-ggplot(df1,aes(x=fv,y=rs))+geom_point(shape = 21)+geom_line(data=df2,color="red")
    #+scale_y_continuous(limits = c(-0.00000000000001,0.00000000000001))
    return(plot1)
    },
  resid=function(){
    return(residuals)
  },
  pred=function(){
    return(fitted_value)
  },
  coef=function(){
    return(reg_coe[,1])
  },
  summary=function(){
    pv<-pt(abs(t_value),df,lower.tail = FALSE)
    dataframe<-data.frame(round(reg_coe[,1],5),round(sqrt(diag(var_reg_coe)),5),round(t_value,2),pv)
    names(dataframe)<-c("Estimate","Std.Error","t value","p value")
    print(dataframe)
    cat("\nResidual standard error:",round(sqrt(res_var),4),"on",df,"degress of freedom")
    #return(df)
  }
))
#l1<-linreg$new(formula=Sepal.Length~Sepal.Width+Petal.Length,data=iris)
# create instance using class

a123<-lm(formula=Petal.Length~Species,data=iris)
summary.lm(a123)

library(ggplot2)
#l1<-linreg$new(Petal.Length~Species, data=iris)
l1<-linreg$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris)
l1$df
l1$summary()



l1$calculate_t_value()









