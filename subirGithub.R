

#library(usethis)

#usethis::use_git()
#usethis::use_github()



git add .
git commit -m "Slides_13"
git push origin main




# Esto lo utilicé para hacer conexión una vez instalado Git
# git config --global user.email "juniorjb5@hotmail.com"



remotes::install_github("jhelvy/xaringanBuilder", dependencies = TRUE)

library(xaringanBuilder)


#build_pdf("Presentation_ICEE_2022.Rmd", complex_slides = TRUE, partial_slides = TRUE)
#build_pdf("Presentation_ICEE_2022.html", complex_slides = TRUE, partial_slides = TRUE)


build_pdf("PDF_ICEE_2022.html", complex_slides = TRUE, partial_slides = TRUE)


.center[<img src="img/moti1.jpg" width="1500px" />]
.center[<img src="img/literatura.JPEG" width="820px" />]
.center[<img src="img/fase1.JPG" width="700px" />]
.center[<img src="img/obj1.JPG" width="900px" />]
.center[<img src="img/obj2.JPG" width="900px" />]
.center[<img src="img/obj3.JPG" width="900px" />]
.center[<img src="img/obj4.JPG" width="900px" />]
.pull-left-narrow[.center[<img src="img/Entramado.PNG" width="50px"/>]]
.center[<img src="img/NAJEF.JPG" width="50px"/>]

#Color Verde
#23373B


<div class="my-logo-center"></div>
  
  
  <div class="my-logo-right"></div>