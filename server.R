server=function(input, output, session) {
  output$plot1 <- renderPlot({
    a=barplot(izh[,input$gorod1],main = "1 Количество поступающих по годам", ylab=input$gorod1, xlab="Год",  names.arg = izh[,1], col="orange")
    text(a,izh[,input$gorod1], labels =izh[,input$gorod1])})
  output$plot2 <- renderPlot({
    b=barplot(izh[,input$gorod2], main = "2 Количество поступивших по годам", ylab=input$gorod2, xlab="Год", names.arg = izh[,1], col="orange")
    text(b,izh[,input$gorod2], labels =izh[,input$gorod2])})
  output$plot3 <- renderPlot({ 
    barplot(izh.T[,input$god1],main = "3 Количество поступающих по городам", 
            ylab=input$god1,xlab="Количество поступающих", names.arg = c("И","С",
                                                                         "В", "М", "Г", "К", "Р", "О"), 
            col="orange", horiz = TRUE)
    legend("topright",xpd=TRUE,c("И-Ижевск", "С-Сарапул", "В-Воткинск", "М-Можга", "Г-Глазов", "К-Камбарка", "Р-Районы УР", "О-Остальное"))})
  output$plot31 <- renderPlot({ 
    barplot(izh1.T[,input$god3],main = "4 Количество поступивших по городам", 
            ylab=input$god3,xlab="Количество поступивших",
            names.arg = c("И","С","В", "М", "Г", 
                          "К", "Р", "О"), col="orange", horiz = TRUE)
    legend("topright",xpd=TRUE,c("И-Ижевск", "С-Сарапул", "В-Воткинск", "М-Можга", "Г-Глазов", "К-Камбарка", "Р-Районы УР", "О-Остальное"))})
  output$plot4<- renderPlot({
    f=barplot(iz[,input$god2], 
              main = "5 Количество поступивших и не поступивших", xlab = "Город", ylab=input$god2,
              names.arg=c("Ижевск", "Ижевск", "Сарапул","Сарапул", "Воткинск", "Воткинск","Можга","Можга", "Глазов", "Глазов","Камбарка", "Камбарка",
                          "Районы УР","Районы УР", "Остальное", "Остальное"),col = c("green","red"))
    text(f,iz[,input$god2], labels =iz[,input$god2])
    legend("topright", xpd=TRUE, c("Поступили","Не поступили"),
           fill = c("green","red"))})
  output$matplot <- renderPlot({ 
    matplot(god, cbind(izh[,input$gorod1],izh[,input$gorod2]),type="b",
            main = "6 Сравнение абитуриентов и студентов",
            ylab=input$gorod1, xlab="Год", 
            col=c("red","green"),pch=16,xlim=c(1997,2021),lty=c(1,1))
    legend("topright",xpd=TRUE,legend=c("Поступали","Поступили"),
           fill = c("red","green"))})
  output$info <- renderText({
    paste0("x=", input$plot_click$x, "\ny=",  input$plot_click$y)})
  output$matplot1<-renderPlot({
    matplot(god, cbind(izh[,2],izh[,4],izh[,6],izh[,8],izh[,10],
                       izh[,12],izh[,14],izh[,16]),main = "7 Сравнение поступающих по городам",
            type="b",pch=16, ylab="Поступали по городам",
            xlab="Год",col=c("red","green","black","251100","purple","orange",
                             "blue","brown"), lty=c(1,1))
    legend("topright",xpd=TRUE,c("Ижевск", "Сарапул", "Воткинск", "Можга", "Глазов", "Камбарка", "Районы УР", "Остальное"),
           fill = c("red","green","black","251100","purple","orange",
                    "blue","brown"))})
  output$info1 <- renderText({
    paste0("x=", input$plot_click1$x, "\ny=",  input$plot_click1$y)})
  output$matplot2<-renderPlot({
    matplot(god, cbind(izh[,3],izh[,5],izh[,7],izh[,9],izh[,11],
                       izh[,13],izh[,15],izh[,17]),main = "8 Сравнение поступивших по городам",
            type="b",pch=16, ylab="Поступали по городам",
            xlab="Год",col=c("red","green","black","251100","purple","orange","blue","brown"),
            lty=c(1,1))
    legend("topright",xpd=TRUE,c("Ижевск", "Сарапул", "Воткинск", "Можга", "Глазов", "Камбарка", "Районы УР", "Остальное"),
           fill = c("red","green","black","251100","purple","orange",
                    "blue","brown"))})
  output$info2 <- renderText({
    paste0("x=", input$plot_click2$x, "\ny=",  input$plot_click2$y)})
  output$pie <- renderPlot({
    pie(izh.T[,input$god1], main="9 Процентное соотношение поступающих",col = rainbow(8), labels=paste0(round(100 * izh.T[,input$god1]/sum(izh.T[,input$god1]), 2), "%"))
    legend("topleft", legend = c("Ижевск", "Сарапул", "Воткинск", "Можга", "Глазов", "Камбарка", "Районы УР", "Остальное"),
           fill=rainbow(8))})
  output$pie1 <- renderPlot({
    pie(izh1.T[,input$god3],main="10 Процентное соотношение поступивших", col = rainbow(8), labels=paste0(round(100 * izh1.T[,input$god3]/sum(izh1.T[,input$god3]), 2), "%"))
    legend("topleft", legend = c("Ижевск", "Сарапул", "Воткинск", "Можга", "Глазов", "Камбарка", "Районы УР", "Остальное"), 
           fill =rainbow(8) )})
  output$plot45<- renderPlot({
    i=barplot(wet,
              main = "11 Распределение выпускников УР", xlab = "Год", ylab='Количество',
              col = c("green","red"))
    legend("topright", xpd=TRUE, c("Не поступающие","Поступающие"),
           fill = c("green","red"))})
  output$matplot3 <- renderPlot({ 
    matplot(nm[,1], cbind(nm[,input$gorod3],izhy[,input$gorod4]),type="b",
            main = "12 Сравнение населения и абитуриентов УР",
            ylab=input$gorod3, xlab="Год", 
            col=c("blue","green"),pch=16,lty=c(1,1))
    legend("topright",xpd=TRUE,legend=c("Население","Поступали"),
           fill = c("blue","green"))})
  output$info3 <- renderText({
    paste0("x=", input$plot_click3$x, "\ny=",  input$plot_click3$y)})
  output$matplot4<- renderPlot({ 
    matplot(izhevsk1[,1],
            cbind(izhevsk1[,input$inst], sarap1[,input$inst], votk1[,input$inst],
                  mozhga1[,input$inst], glaz1[,input$inst], kamb1[,input$inst],
                  rajon1[,input$inst], ostal1[,input$inst], vsee1[,input$inst]),
            main = "13 Сравнение абитуриентов по факультетам",
            ylab=input$inst, xlab="Год", 
            col= rainbow(9),pch=16,lty=c(1,1), type="b")
    legend("topleft",xpd=TRUE,legend=c("Ижевск","Сарапул","Воткинкс", "Можга",
                                       "Глазов", "Камбарка", "Районы УР", "Остальное", "По УдГУ"),
           fill =rainbow(9))})
  output$info4<- renderText({
    paste0("x=", input$plot_click4$x, "\ny=",  input$plot_click4$y)})
  output$matplot5<- renderPlot({ 
    matplot(izhevsk2[,1],
            cbind(izhevsk2[,input$inst], sarap2[,input$inst], votk2[,input$inst],
                  mozhga2[,input$inst], glaz2[,input$inst], kamb2[,input$inst],
                  rajon2[,input$inst], ostal2[,input$inst], vsee2[,input$inst]),
            main = "14 Сравнение студентов по факультетам",
            ylab=input$inst, xlab="Год", 
            col= rainbow(9),pch=16,lty=c(1,1), type="b")
    legend("topleft",xpd=TRUE,legend=c("Ижевск","Сарапул","Воткинкс", "Можга",
                                       "Глазов", "Камбарка", "Районы УР", "Остальное", "По УдГУ"),
           fill =rainbow(9))})
  output$info5<- renderText({
    paste0("x=", input$plot_click5$x, "\ny=",  input$plot_click5$y)})
  output$matplot6 <- renderPlot({ 
    matplot(izhevsk1[,1], cbind(izhevsk1[,input$inst],izhevsk2[,input$inst]),type="b",
            main = "15 Сравнение абитуриентов и студентов по факультетам из города Ижевск",
            ylab=input$inst, xlab="Год", 
            col=c("red","green"),pch=16,xlim=c(1997,2021),lty=c(1,1))
    legend("topleft",xpd=TRUE,legend=c("Поступали","Поступили"),
           fill = c("red","green"))})
  output$info6 <- renderText({
    paste0("x=", input$plot_click6$x, "\ny=",  input$plot_click6$y)})
  output$matplot7 <- renderPlot({ 
    matplot(izhevsk1[,1], cbind(sarap1[,input$inst],sarap2[,input$inst]),type="b",
            main = "16 Сравнение абитуриентов и студентов по факультетам из города Сарапул",
            ylab=input$inst, xlab="Год", 
            col=c("red","green"),pch=16,xlim=c(1997,2021),lty=c(1,1))
    legend("topleft",xpd=TRUE,legend=c("Поступали","Поступили"),
           fill = c("red","green"))})
  output$info7 <- renderText({
    paste0("x=", input$plot_click7$x, "\ny=",  input$plot_click7$y)})
  output$matplot8 <- renderPlot({ 
    matplot(izhevsk1[,1], cbind(votk1[,input$inst],votk2[,input$inst]),type="b",
            main = "17 Сравнение абитуриентов и студентов по факультетам из города Воткинск",
            ylab=input$inst, xlab="Год", 
            col=c("red","green"),pch=16,xlim=c(1997,2021),lty=c(1,1))
    legend("topleft",xpd=TRUE,legend=c("Поступали","Поступили"),
           fill = c("red","green"))})
  output$info8 <- renderText({
    paste0("x=", input$plot_click8$x, "\ny=",  input$plot_clic8$y)})
  output$matplot9 <- renderPlot({ 
    matplot(izhevsk1[,1], cbind(glaz1[,input$inst],glaz2[,input$inst]),type="b",
            main = "18 Сравнение абитуриентов и студентов по факультетам из города Глазов",
            ylab=input$inst, xlab="Год", 
            col=c("red","green"),pch=16,xlim=c(1997,2021),lty=c(1,1))
    legend("topleft",xpd=TRUE,legend=c("Поступали","Поступили"),
           fill = c("red","green"))})
  output$info9 <- renderText({
    paste0("x=", input$plot_click9$x, "\ny=",  input$plot_click9$y)})
  output$matplot10 <- renderPlot({ 
    matplot(izhevsk1[,1], cbind(kamb1[,input$inst], kamb2[,input$inst]),type="b",
            main = "19 Сравнение абитуриентов и студентов по факультетам из города Камбарка",
            ylab=input$inst, xlab="Год", 
            col=c("red","green"),pch=16,xlim=c(1997,2021),lty=c(1,1))
    legend("topleft",xpd=TRUE,legend=c("Поступали","Поступили"),
           fill = c("red","green"))})
  output$info10 <- renderText({
    paste0("x=", input$plot_click10$x, "\ny=",  input$plot_click10$y)})
  output$matplot11 <- renderPlot({ 
    matplot(izhevsk1[,1], cbind(rajon1[,input$inst],rajon2[,input$inst]),type="b",
            main = "20 Сравнение абитуриентов и студентов по факультетам из районов УР",
            ylab=input$inst, xlab="Год", 
            col=c("red","green"),pch=16,xlim=c(1997,2021),lty=c(1,1))
    legend("topleft",xpd=TRUE,legend=c("Поступали","Поступили"),
           fill = c("red","green"))})
  output$info11 <- renderText({
    paste0("x=", input$plot_click11$x, "\ny=",  input$plot_click11$y)})
  output$matplot12 <- renderPlot({ 
    matplot(izhevsk1[,1], cbind(ostal1[,input$inst],ostal2[,input$inst]),type="b",
            main = "21 Сравнение абитуриентов и студентов по факультетам из остальных регионов и стран",
            ylab=input$inst, xlab="Год", 
            col=c("red","green"),pch=16,xlim=c(1997,2021),lty=c(1,1))
    legend("topleft",xpd=TRUE,legend=c("Поступали","Поступили"),
           fill = c("red","green"))})
  output$info12 <- renderText({
    paste0("x=", input$plot_click12$x, "\ny=",  input$plot_click12$y)})
  output$matplot13 <- renderPlot({ 
    matplot(izhevsk1[,1], cbind(vsee1[,input$inst],vsee2[,input$inst]),type="b",
            main = "22 Сравнение абитуриентов и студентов по факультетам по ВУЗу",
            ylab=input$inst, xlab="Год", 
            col=c("red","green"),pch=16,xlim=c(1997,2021),lty=c(1,1))
    legend("topleft",xpd=TRUE,legend=c("Поступали","Поступили"),
           fill = c("red","green"))})
  output$info13 <- renderText({
    paste0("x=", input$plot_click13$x, "\ny=",  input$plot_click13$y)})
  output$downloadReport <- downloadHandler(
    filename = function() {
      paste('my-report', sep = '.', switch(
        input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
      ))
    },
    
    content = function(file) {
      src <- normalizePath('report.Rmd')
      
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'report.Rmd', overwrite = TRUE)
      
      library(rmarkdown)
      out <- render('report.Rmd', switch(
        input$format,
        PDF = pdf_document(), HTML = html_document(), Word = word_document()
      ))
      file.rename(out, file)
    }
  )
  
}