# CV

This repo uses [Daniel Anderson's](https://github.com/datalorax) template and renders a data-driven CV. 

Steps for rendering:

* adjust content excel files

* adjust base .Rmd file

* open R studio project

* run 'pagedown::chrome_print('anderson-cv.Rmd', output = "out.pdf")

* to edit and re-run, quit re-studio and delete the pdf. Then re-render