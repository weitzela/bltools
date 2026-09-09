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

#' Add dots or values to heatmap cells
#'
#' Annotate significant ComplexHeatmap Heatmap cells with a dot, or print values in them.
#' Supplying `label_mat` switches from drawing dots to printing that matrix's values.
#' The returned function is passed to either the `layer_fun` or the `cell_fun` argument of `ComplexHeatmap::Heatmap()`.
#'
#' @export
#' @param value_mat provide a significance matrix the same dimensions and order as the matrix used to plot values. This matrix decides which cells get annotated.
#' @param threshold a significance score to determine what cells to annotate. Set to `NULL` to annotate every non-`NA` cell (e.g. to print a value in every cell).
#' @param dot_pt_size size of dot to print on heatmap, in points. Only used when `label_mat` is `NULL`.
#' @param fun value of either layer or cell. "layer" is vectorised over the cells of a slice and is faster; "cell" is called once per cell.
#' @param label_mat matrix of values to print in the annotated cells, same dimensions and order as `value_mat`. `NULL` (default) draws dots instead. Pass `value_mat` itself to print the significance values, or the plotted matrix to print e.g. correlation coefficients only where p < threshold. May be numeric or character (e.g. pre-computed significance stars).
#' @param label_fmt how to turn `label_mat` values into text: either a `sprintf()` format string (e.g. `"%.2f"`) or a function taking a vector of values and returning a character vector. Left `NULL`, a numeric `label_mat` whose values are all within 1 in magnitude (p-values, correlation coefficients) is formatted as `"%.2f"`; anything else uses `format()` defaults.
#' @param gp a `grid::gpar()` of graphical parameters, merged over the default `gpar(fontsize = 6)`. Font parameters are dropped when drawing dots so `dot_pt_size` stays in control of the dot size.
#' @param ... individual `gpar()` parameters (e.g. `fontsize = 8`, `col = "white"`, `fontface = "bold"`), merged over `gp`.
#' @examples
#' # dots where p < 0.01 (original behaviour)
#' # Heatmap(r_mat, layer_fun = hmSigDots(p_mat, 0.01))
#' # print the correlation coefficient in every significant cell
#' # Heatmap(r_mat, layer_fun = hmSigDots(p_mat, 0.05, label_mat = r_mat,
#' #                                      label_fmt = "%.2f", fontsize = 7))
#' # print significance stars in every cell
#' # Heatmap(r_mat, cell_fun = hmSigDots(p_mat, threshold = NULL, fun = "cell",
#' #                                     label_mat = p_mat, label_fmt = gtools::stars.pval))
hmSigDots = function(value_mat, threshold = 0.05, dot_pt_size = 2, fun = "layer",
                     label_mat = NULL, label_fmt = NULL, gp = grid::gpar(fontsize = 6), ...) {
  # use this function within a complex Heatmap function: e.g., Heatmap(..., layer_fun = hmSigDots(pvalue_mat, 0.01))
  fun = match.arg(fun, c("layer", "cell"))
  value_mat = as.matrix(value_mat)
  if (!is.null(label_mat)) {
    label_mat = as.matrix(label_mat)
    if (!identical(dim(label_mat), dim(value_mat))) stop("`label_mat` must have the same dimensions as `value_mat`.")
    # fractional values (p-values, correlations) print unreadably wide with format() defaults
    if (is.null(label_fmt) && is.numeric(label_mat) && any(!is.na(label_mat)) &&
        max(abs(label_mat), na.rm = TRUE) <= 1) label_fmt = "%.2f"
  }

  # merge gpar defaults < gp < ..., so only the parameters to change need to be given
  .gp = list(fontsize = 6)
  .gp[names(gp)] = gp
  .dots = list(...)
  .gp[names(.dots)] = .dots
  # font parameters rescale character-based points, so keep them out of the dot gpar
  if (is.null(label_mat)) .gp = .gp[setdiff(names(.gp), c("fontsize", "fontface", "fontfamily", "cex", "lineheight"))]
  .gp = do.call(grid::gpar, .gp)

  print_value = function(x, y, cell_value) {
    if (is.null(cell_value)) {
      grid::grid.points(x, y, size = grid::unit(dot_pt_size, "pt"), pch = 16, gp = .gp)
    } else {
      if (is.function(label_fmt)) cell_value = as.character(label_fmt(cell_value))
      else if (!is.null(label_fmt)) cell_value = sprintf(label_fmt, cell_value)
      else cell_value = format(cell_value, trim = TRUE)
      grid::grid.text(cell_value, x, y, gp = .gp)
    }
  }

  if (fun == "layer") {
    .layer_fun = function(j, i, x, y, width, height, fill) {
      v_P = ComplexHeatmap::pindex(value_mat, i, j)
      c_1 = !is.na(v_P)
      if (!is.null(threshold)) c_1 = c_1 & (v_P < threshold)
      if (is.null(label_mat)) {
        # subsetting a unit vector with nothing selected is an error, so check first
        if (any(c_1)) print_value(x[c_1], y[c_1], NULL)
      } else {
        v_L = ComplexHeatmap::pindex(label_mat, i, j)
        c_1 = c_1 & !is.na(v_L)
        if (any(c_1)) print_value(x[c_1], y[c_1], v_L[c_1])
      }
    }
    return(.layer_fun)
  } else {
    .cell_fun = function(j, i, x, y, width, height, fill) {
      v_P = value_mat[i, j]
      if (is.na(v_P) || (!is.null(threshold) && !(v_P < threshold))) return(invisible(NULL))
      if (is.null(label_mat)) {
        print_value(x, y, NULL)
      } else if (!is.na(label_mat[i, j])) {
        print_value(x, y, label_mat[i, j])
      }
    }
    return(.cell_fun)
  }
}

#' Hmisc heatmap wrapper function
#' 
#' @export
chHeatmap = function(mat, cell_mm=3, col_and_row_name_size = 8, .col = circlize::colorRamp2(c(-1, -0.5, 0, 0.5, 1), c("#444EB8", "#A2A7DB", "white", "#D38A90", "#B8444E")), .name = "r", heatmap_legend_param = NULL, ...) {
  # creates the most common heatmap I typically make, additional things can be set when calling the ComplexHeatmap::draw function
  # input a list that results from using the Hmisc::rcorr function. All matrices must be the same size and have the same row and column names
  # rcorr_ls = replaceNAwith1(rcorr_ls)
  if (length(col_and_row_name_size) == 1) col_and_row_name_size = c(col_and_row_name_size, col_and_row_name_size)
  if (length(cell_mm) == 1) cell_mm = c(cell_mm, cell_mm)
  
  hm = ComplexHeatmap::Heatmap(mat, col = .col,
                               heatmap_legend_param = c(list(title = .name), heatmap_legend_param),
                               width = ncol(mat)*grid::unit(cell_mm[1], "mm"), height = nrow(mat)*grid::unit(cell_mm[2], "mm"),
                               row_names_gp = grid::gpar(fontsize = col_and_row_name_size[1]), 
                               column_names_gp = grid::gpar(fontsize = col_and_row_name_size[2]), ...)
  return(hm)
}

#' annotate cluster size on complexheatmap
#' 
#' @export
clusterSizeAnno = function(x, col = "bisque3", .text_size = 6, .width = 0.3, .name_size = 6) {
  hm_anno = anno_simple(x, col = circlize::colorRamp2(c(0, max(x)), c("white", col)), 
                        pch = as.character(x), pt_size = unit(.text_size, "pt"), width = unit(.width, "cm"), which = "row", gp = gpar(lwd = 0))
  anno_obj = rowAnnotation(`Cluster Size` = hm_anno, 
                           show_legend = FALSE, 
                           annotation_name_gp = gpar(fontsize = .name_size))
  return(anno_obj)
}

#' Wrapper function for fgsea ranked geneset enrichment analysis
#' 
#' @export
fgsea_wrapper = function(input_genes, geneset_ls, geneset_info, scoreType = "pos") {
  # input_genes should be a vector of statistical values with names as the ensembl gene ID
  # values should be something like log10pvalue, where the more significant values are greater
  # by default, the scoreType is set to "pos" following the assumption that genes with greater values are more likely to enrich the set
  pathway_res = fgsea::fgsea(geneset_ls, input_genes, minSize = 10, maxSize = 500, scoreType = scoreType) |> 
    left_join(geneset_info |> select(pathway = Geneset.ID, Geneset.Type, Description), by = "pathway") |> 
    relocate(Geneset.Type, pathway, Description) |> 
    mutate(padj = p.adjust(pval, "BH"), .by = "Geneset.Type") |> 
    arrange(padj) |> 
    as.data.frame() |> 
    dplyr::rename("ID" = "pathway", "pvalue" = "pval", "p.adjust" = "padj") |> 
    `attr<-`("input_genes", input_genes)
  return(pathway_res)
}

#' Over-representation analysis
#' 
#' @export
#' @param bkg_genes character vector of all genes expressed in the dataset
#' @param geneset_df dataframe form of geneset IDs and genes. e.g., gs_ls$genesets_and_genes
#' @param geneset_info dataframe with info such as names, descriptions, database, etc. gs_ls$geneset_info
enricher_wrapper = function(target_genes, bkg_genes, geneset_df, geneset_info) {
  if (length(bkg_genes) == 0) stop("Provide a background list with genes.")
  # function created 12/10/2024 to consistently and easily carry out simple over representation analysis using a target and background list, referring to the custom categories I prepared previously. because this includes categories from multiple GO and KEGG databases, the results are then grouped by database and FDR adjusted
  if (!exists("categories")) categories <<- readRDS(misc$data.files$info$chipenrich_custom_categories)
  enrich_res = clusterProfiler::enricher(target_genes, TERM2GENE = geneset_df |> select(-1) |> `colnames<-`(c("term", "gene")), TERM2NAME = geneset_info |> select(2:3) |> `colnames<-`(c("term", "name")), universe = bkg_genes, pvalueCutoff = 0.05, qvalueCutoff = 1, minGSSize = 10, maxGSSize = 500)@result
  # FDR adjust by database
  enrich_res = enrich_res |> 
    mutate(db = pull(geneset_info, Geneset.Type, name = "Geneset.ID")[ID], .before  = "ID") |> 
    group_by(db) |> 
    # replace p.adjust column to FDR corrects for each ontology trial, instead of combining the adjustment across multiple GO and KEGG databases
    mutate(p.adjust = p.adjust(pvalue, "BH"), .after = "p.adjust") |> 
    ungroup() |> 
    arrange(p.adjust)
  return(enrich_res)
}