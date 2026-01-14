#' Export figures in PDF files and then embed fonts
#' 
#' A note about pdf saving issues - if there are kerning or sizing issues, use this function so that plots are saved with PDFs then the font is embedded in the file
#' This function takes the steps to save the plot as a pdf with the arial font, and then embedding the font in the file. this should not change the sizing of fonts and should return normal kerning. it seems like with any PDF, the font might look a little more "bold" than when saved as a png
#' 
#' @export
#' @param file Destination filename. Must be a full path, not the ~/ if referring to a different file location from the working directory.
#' @param plot_fxn A function that prints the plot. E.g., `plot(saved_ggplot)`
#' @param width Width of file to save, in unit inches.
#' @param height Height of file to save, in unit inches.
#' @examples
#' # example code
#' @keywords internal
pdfEmbed = function(file, plot_fxn, width, height, family = "ArialMT", device = "quartz", bg = "white", ...) {
  # A note about pdf saving issues - if there are kerning or sizing issues, use this function so that plots are saved with PDFs then the font is embedded in the file
  # this function takes the steps to save the plot as a pdf with the arial font, and then embedding the font in the file. this should not change the sizing of fonts and should return normal kerning. it seems like with any PDF, the font might look a little more "bold" than when saved as a png
  if (device == "pdf") {
    grDevices::pdf(file, width = width, height = height, family = family, ...)
  } else if (device == "quartz") {
    if (is.null(quartzFonts()$Arial)) quartzFonts(Arial = quartzFont(c("ArialMT","Arial-BoldMT","Arial-ItalicMT","Arial-BoldItalicMT")))
    grDevices::quartz(file = file, type = "pdf", family = "Arial", width = width, height = height, bg = bg, ...)
  }
  plot_fxn
  dev.off()
  if (!nzchar(Sys.getenv("R_GSCMD", unset = ""))) {
    Sys.setenv(R_GSCMD   = "/opt/homebrew/bin/gs", 
               GS_FONTPATH="/System/Library/Fonts:/System/Library/Fonts/Supplemental")
  }
  embedFonts(file, outfile = file)
}