# linreg RC Class Object

#' linreg RC Class Object
#' @param formula formula of regression model
#' @param data data of regression model
#' @examples
#' data(iris)
#' l1 <- linreg$new(formula = Petal.Length~Sepal.Width+Sepal.Length, data = iris)
#' l1$print()
#' @export linreg
#' @exportClass linreg

linreg<-setRefClass("linreg",fields=list(formula='formula',data='data.frame',reg_coe='matrix',fitted_value='matrix',residuals='matrix',df='numeric',res_var='matrix',var_reg_coe='matrix',t_value='matrix',data_name = "character"),methods=list(
  initialize = function(formula,data){
    library(ggplot2)
    formula<<-formula
    data<<-data
    data_name <<- deparse(substitute(data))
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
  print = function(){
    cat("linreg(formula = ", format(formula), ", data = ",data_name,")\n", sep = "")
    argnames <- sys.call()
    print.table(t(reg_coe))
  },
  plot=function(){
    d<-c()
    for(i in 1:nlevels(factor(fitted_value))){
      pos<-which(fitted_value==levels(factor(fitted_value))[i])
      #m<-mean(residuals[pos[1]:pos[length(pos)]])
      m<-median(residuals[pos[1]:pos[length(pos)]])
      #m<-mean(l1$residuals(pos[1]:pos[length(pos)]))
      d<-c(d,m)
    }
    
    max1<-max(residuals)
    max2<-max(residuals[-max1])
    min<-min(residuals)
    topAndBotton<-c(max1,max2,min)
    
    axe_x<-as.numeric(levels(factor(fitted_value)))
    df1<-data.frame(fv=fitted_value,rs=residuals)
    names(df1)<-c("fv","rs")
    df2<-data.frame(fv=axe_x,rs=d)
    
    #plot1<-ggplot(df1,aes(x=fv,y=rs))+geom_point(shape = 21)+geom_line(data=df2,color="red")
    plot1<-ggplot(df1,aes(x=fv,y=rs))+geom_point(shape = 21)+stat_smooth(method = 'lm',se=FALSE,colour="red")
    plot1<-plot1+labs(title = "Residuals vs Fitted",x="Fitted Values",y="Residuals")+theme(plot.title=element_text(hjust=0.5))

    
    
    df3<-data.frame(fv=fitted_value,stand_res=sqrt(abs(residuals)))
    plot2<-ggplot(df3,aes(x=fitted_value,y=sqrt(abs(residuals))))
    plot2<-plot2+geom_point(shape = 21)+stat_smooth(method = 'lm',se=FALSE,colour="red")+labs(title = "Scale−Location",x="Fitted Values")+theme(plot.title=element_text(hjust=0.5))
    
    
    return(plot2)
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
  summary = function(){
    standard_error <- sqrt(diag(var_reg_coe))
    p_value <- 2 * pt(abs(t_value), df, lower.tail = FALSE)
    summary_result <- cbind(reg_coe, standard_error, t_value, paste(p_value, "***"))
    colnames(summary_result) <- c("Estimate", "Std. Error", "t value", "p value")
    # colnames(summary_result) <- NULL
    format(summary_result, trim = FALSE, scientific = TRUE, justify = "l")
    print.table(summary_result)
    cat("Residual standard error:", sqrt(res_var),"on",df,"degrees of freedom")
  }
))
#l1<-linreg$new(formula=Sepal.Length~Sepal.Width+Petal.Length,data=iris)
# create instance using class

#a123<-lm(formula=Petal.Length~Species,data=iris)
#summary.lm(a123)

library(ggplot2)
#l1<-linreg$new(Petal.Length~Species, data=iris)
l1<-linreg$new(Petal.Length~Species, data=iris)
l1$plot()
l1$residuals
l1$summary()
