# linreg RC Class Object using QR Decomposition 

#' linreg RC Class Object using QR Decomposition 
#' @param formula formula of regression model
#' @param data data of regression model
#' @examples
#' data("iris")
#' linreg_mod <- linreg_qr$new(formula = Petal.Length~Sepal.Width+Sepal.Length, data = iris)
#' linreg_mod$print()
#' @export linreg_qr
#' @exportClass linreg_qr

linreg_qr <- setRefClass("linreg_qr",
                      fields=list(formula = 'formula',
                                  data = 'data.frame',
                                  reg_coe = 'matrix',
                                  fitted_value = 'matrix',
                                  residuals = 'matrix',
                                  df = 'numeric',
                                  res_var = 'matrix',
                                  var_reg_coe = 'matrix',
                                  t_value = 'matrix',
                                  .data_name = "character"),
                      methods = list(
                        initialize = function(formula,data){
                          formula <<- formula
                          data <<- data
                          .data_name <<- deparse(substitute(data))
                          calculate_Regressions_coefficients()
                          calculate_the_fitted_value()
                          calculate_the_residuals()
                          calculate_degress_of_freedom()
                          calculate_residual_variance()
                          calculate_var_reg_coe()
                          calculate_t_value()
                        },
                        get_matrix_x = function(){
                          return(model.matrix(formula, data))
                        },
                        get_matrix_y = function(){
                          y = as.matrix(data[all.vars(formula)[1]])
                          return(y)
                        },
                        get_qr = function(){
                          QR = qr(get_matrix_x())
                          return (QR)
                        },
                        get_q = function(){
                          Q = qr.Q(get_qr())
                          return(Q)
                        },
                        get_r = function(){
                          R = qr.R(get_qr())
                          return(R)
                        },
                        calculate_Regressions_coefficients = function(){
                          y = get_matrix_y()
                          QR = get_qr()
                          # reg_coe <<- solve.qr(QR, y)
                          reg_coe <<- qr.coef(QR, y)
                        },
                        calculate_the_fitted_value = function(){
                          QR = get_qr()
                          y = get_matrix_y()
                          fitted_value <<- qr.fitted(QR, y, k= QR$rank)
                        },
                        calculate_the_residuals = function(){
                          QR = get_qr()
                          y = get_matrix_y()
                          residuals <<- qr.resid(QR, y)
                        },
                        calculate_degress_of_freedom = function(){
                          x<-get_matrix_x()
                          df <<- nrow(x)-ncol(x)
                        },
                        calculate_residual_variance = function(){
                          res_var <<- (t(residuals) %*% residuals) / df
                        },
                        calculate_var_reg_coe = function(){
                          R = get_r()
                          var_reg_coe <<- chol2inv(R) * res_var[1,1]
                        },
                        calculate_t_value = function(){
                          t_value <<- reg_coe / sqrt(diag(var_reg_coe))
                        },
                        print = function(){
                          cat("linreg(formula = ", format(formula), ", data = ", .data_name,")\n", sep = "")
                          argnames <- sys.call()
                          print.table(t(reg_coe))
                        },
                        plot = function(){
                          d<-c()
                          for(i in 1:nlevels(factor(fitted_value))){
                            pos<-which(fitted_value==levels(factor(fitted_value))[i])
                            m<-median(residuals[pos[1]:pos[length(pos)]])
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
                          
                          plot1<-ggplot(df1,aes(x=fv,y=rs))+geom_point(shape = 21)+stat_smooth(method = 'lm',se=FALSE,colour="red")
                          plot1<-plot1+labs(title = "Residuals vs Fitted",x="Fitted Values",y="Residuals",caption=format(formula))+theme(plot.title=element_text(hjust=0.5))
                          
                          df3<-data.frame(fv=fitted_value,stand_res=sqrt(abs(residuals)))
                          plot2<-ggplot(df3,aes(x=fitted_value,y=sqrt(abs(residuals))))
                          plot2<-plot2+geom_point(shape = 21)+stat_smooth(method = 'lm',se=FALSE,colour="red")+labs(title = "Scale-Location",x="Fitted Values",caption=(format(formula)))+theme(plot.title=element_text(hjust=0.5),plot.caption =element_text(hjust=0.5))
                          
                          return(list(plot1,plot2))
                        },
                        resid = function(){
                          return(as.vector(residuals))
                        },
                        pred = function(){
                          return(fitted_value)
                        },
                        coef = function(){
                          result = as.vector(reg_coe)
                          names(result) = colnames(reg_coe)
                          return(result)
                        },
                        summary = function(){
                          standard_error <- sqrt(diag(var_reg_coe))
                          p_value <- 2 * pt(abs(t_value), df, lower.tail = FALSE)
                          summary_result <- cbind(reg_coe, standard_error, t_value, paste(p_value, "***"))
                          colnames(summary_result) <- c("Estimate", "Std. Error", "t value", "p value")
                          format(summary_result, trim = FALSE, scientific = TRUE, justify = "l")
                          print.table(summary_result)
                          cat("Residual standard error:", sqrt(res_var),"on",df,"degrees of freedom")
                        }
                      )
)
