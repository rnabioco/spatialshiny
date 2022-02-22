# some other code for webpage functions
jscodega <- '
$(document).on("shiny:sessioninitialized",function(){
$("#Find").on("click", function() {
  ga("send", "event", "button", "Find", $("#geneID").val());
});
});
'

jscode <- '
$(function() {
  var $els = $("[data-proxy-click]");
  $.each(
    $els,
    function(idx, el) {
      var $el = $(el);
      var $proxy = $("#" + $el.data("proxyClick"));
      $el.keydown(function (e) {
        if (e.keyCode == 13) {
          $proxy.click();
        }
      });
    }
  );
});
'

jsResetCode <- "shinyjs.reset = function() {history.go(0)}"

# Define UI for application that draws the UMAP
ui <- fluidPage(
  title = "PFA Ependymoma Spatial Transcriptomics Atlas",
  theme = shinytheme(set_shinytheme),
  tags$style("
    .nav-tabs{
      font-weight:bold;
      font-size:13px;
    }
# .btn-default, .btn-options{
# display:inline-block;
# padding:0.3em 1.2em;
# margin:0 0.1em 0.1em 0;
# border:0.16em solid rgba(255,255,255,0);
# border-radius:2em;
# box-sizing: border-box;
# text-decoration:none;
# font-family:'Roboto',sans-serif;
# font-weight:300;
# color:#FFFFFF;
# text-shadow: 0 0.04em 0.04em rgba(0,0,0,0.35);
# text-align:center;
# transition: all 0.2s;
# background-color:#4e9af1;
# }
# .btn-default:hover{
# border-color: rgba(255,255,255,1);
# }
      .checkbox {
        line-height: 20px;
        margin-top: -15px;
        margin-bottom: -15px;
      }
      .header {
        position:fixed;
        z-index:100;
        width: calc(100% - 70px);
      }
      .BioCircosARCTooltip {
        z-index:10
      }
      .BioCircosHISTOGRAMTooltip {
        z-index:10
      }
  "),
  tags$head(tags$style(
    HTML(
      "
        #SIDE {
          background-color: #F8F8F8;
        }",
      ".btn-options {
          background-color: #F8F8F8;
        }",
      ".btn {
          text-transform: unset !important;
        }",
      ".shiny-notification {
        position:fixed;
        top: 10px;
        right: 10px);
      }"
    )
  )),
  tags$head(includeHTML(("www/google-analytics.html"))),
  useShinyjs(),
  introjsUI(),
  use_bs_tooltip(),
  tags$style("
  .introjs-helperLayer {
  background: transparent;
}

.introjs-overlay {
  opacity: 0 !important;
  z-index: 99999999!important;
}

.introjs-helperLayer:before {
  opacity: 0;
  content: '';
  position: absolute;
  width: inherit;
  height: inherit;
  border-radius: 0.5em;
  border: .2em solid rgba(255, 217, 68, 0.8);
  box-shadow: 0 0 0 1000em rgba(0,0,0, .7);
  opacity: 1;
}

.introjs-helperLayer:after {
  content: '';
  left: 0;
  right: 0;
  top: 0;
  bottom: 0;
  position: absolute;
  z-index: 1000;
}

             "),
  tags$style(".mock {position:fixed;width:7%;margin-top: 60px;z-index:10;}"),
  tags$style("
             .download_this{
    margin-right: 1px;
}"),
  tags$script(src = "shinyBS_mod.js"),
  tags$head(tags$script(HTML(jscode))),
  tags$head(tags$script(HTML(jscodega))),
  tags$head(tags$script(HTML(jsResetCode))),
  tags$head(tags$style(
    type = "text/css",
    ".shiny-output-error { visibility: hidden; }",
    ".shiny-output-error:before { visibility: hidden; }",
    ".shiny-output-warning { visibility: hidden; }",
    ".shiny-output-warning:before { visibility: hidden; }"
  )),
  titlePanel(div(
    class = "header", 
    a(
      img(src = "logo.png", alt = "RNA Bioscience Initiative", style = "width:29px;"),
      href = "https://github.com/rnabioco/",
      target="_blank"
    ) %>%
      bs_embed_tooltip("The RNA Bioscience Initiative at the University of Colorado Anschutz Medical Campus", placement = "bottom"),
    span(a(
      strong(apptitle_short),
      href = url,
      target="_blank"
    ) %>%
      bs_embed_tooltip("part of Pediatric Neuro-oncology Cell Atlas project", placement = "bottom"),
    div(
      code(apptitle),
      style = "font-size:15px;margin-top:2px;display:inline"
    )),
    style = "font-size:30px;background-color:rgba(255,255,255,0.5);background-clip:inherit;text-align:center;margin:0px;padding:1px 1px;"
  )),
  fixedPanel(
    style = "z-index:100;",
    right = 10,
    top = 22,
    tags$head(
      tags$style(HTML("#tutorial{background-color:gold;z-index:100;}"))
    ),
    introBox(
      actionButton("tutorial", "", icon = icon("question"), 
                   style = "padding:4px 10px;margin:0px;") %>%
        bs_embed_tooltip("Take a tour through the browser!", placement = "bottom"),
      data.step = 1,
      data.intro = "Welcome to the PFA Spatial Transcriptomics Atlas.<br><br>
      Please hover over buttons, tabs, etc for tips/explanations.",
      data.position = "left"
    )
  ),
  sidebarLayout(
    sidebarPanel(
      tags$head(tags$script('
                                var dimension = [0, 0];
                                $(document).on("shiny:connected", function(e) {
                                    dimension[0] = window.innerWidth;
                                    dimension[1] = window.innerHeight;
                                    Shiny.onInputChange("dimension", dimension);
                                });
                                $(window).resize(function(e) {
                                    dimension[0] = window.innerWidth;
                                    dimension[1] = window.innerHeight;
                                    Shiny.onInputChange("dimension", dimension);
                                });
                            ')),
      id = "SIDE",
      style = "position:fixed;width:16%;margin-top:60px;z-index:50;border:solid 2.3px; 
        border-top-color: #4e9af1;
        border-left-color: #4e9af1;
        border-right-color: #9BE7FF;
        border-bottom-color: #9BE7FF;",
      width = 2,
      div(
        id = "sideall",
        div(
          id = "sidediv",
          tabsetPanel(
            id = "side1",
            tabPanel(
              span(icon("link", class = NULL, lib = "font-awesome"), "Gene",
                   title = "plot expression a specific query gene"
              ),
              br(.noWS = "outside"),
              introBox(
                div(
                  div(
                    style = "display:inline-block;vertical-align:top;width:100%;",
                    tagAppendAttributes(selectizeInput("geneID",
                                                       label = NULL,
                                                       selected = "",
                                                       choices = ""
                    ),
                    `data-proxy-click` = "Find"
                    )
                  ),
                  div(
                    style = "display: inline-block;vertical-align:top; width: 10px;",
                    actionButton("Find", "Find", icon = icon("search"),
                                 style = "padding:3px 9px;float:left;margin:3px;") %>%
                      bs_embed_tooltip("gene symbols accepted", placement = "right")
                  )
                ),
                data.step = 2,
                data.intro = "Query individual gene by symbol.",
                data.position = "bottom"
              )
            ),
            tabPanel(
              span(icon("cogs", class = NULL, lib = "font-awesome"), "Options", title = "additional global options"),
              br(.noWS = "outside"),
              div(
                id = "doLockdiv",
                checkboxInput("doLock", "lock panel order", value = FALSE, width = NULL) %>%
                  bs_embed_tooltip("if unlocked, tabs, sections, and table columns can be dragged and reordered",
                                   placement = "right"
                  )
              ),
              tags$table(
                tags$head(
                  tags$style(HTML("#pval{margin-top: 0px; margin-bottom: -10px; font-size:12px;}"))
                ),
                tags$head(
                  tags$style(HTML("#ncol{margin-top: 0px; margin-bottom: -10px; font-size:12px;}"))
                ),
                tags$head(
                  tags$style(HTML("#plotw{margin-top: 0px; margin-bottom: -10px; font-size:12px;}"))
                ),
                tags$head(
                  tags$style(HTML("#ploth{margin-top: 0px; margin-bottom: -10px; font-size:12px;}"))
                ),
                tags$tr(
                  width = "100%",
                  tags$td(width = "50%", tags$div(style = "font-size:12px;", "plot height")),
                  tags$td(width = "50%", textInput("ploth", NULL, value = plot_height, width = "100px") %>%
                            bs_embed_tooltip("plot height for app (px) and saved pdf (in)", placement = "right"))
                ),
                tags$tr(
                  width = "100%",
                  tags$td(width = "50%", tags$div(style = "font-size:12px;", "plot width")),
                  tags$td(width = "50%", textInput("plotw", NULL, value = plot_width, width = "100px") %>%
                            bs_embed_tooltip("plot width for app (px) and saved pdf (in)", placement = "right"))
                )
              )
            )
          )
        )
      )
    ),
    mainPanel(
      id = "MAIN",
      width = 10,
      style = "z-index:1;margin-top: 60px;",
      tabsetPanel(
        id = "tabMain",
        tabPanel(
          introBox(
            span(icon("pencil-ruler", class = NULL, lib = "font-awesome"),
                 "Feature Plot",
                 title = "Plot expression or category data in 2D UMAP or space"
            ),
            data.step = 3,
            data.intro = "For a query gene, this tab displays the SCT normalized count expression.",
            data.position = "top"
          ),
          value = "Gene_query",
          div(
            id = "sorted",
            div(
              introBox(
                div(
                  selectInput("sel1", NULL, choices = slides, selected = "821_H&E"),
                  style = "border-radius:0px;vertical-align:top;margin:0px;padding-top:10px;padding-bottom:-200px;width:100px;"
                ),
                data.step = 4,
                data.intro = "Choose between umap/H&Eonly/slides.",
                data.position = "right"
              ),
              div(
                uiOutput("dimPlotUI1"),#%>% withLoader(proxy.height = proxy_height),
              ),
            style = "display: inline-block;"
            ),
            div(
              div(
                selectInput("sel2", NULL, choices = slides, selected = "821"),
                style = "border-radius:0;pxvertical-align:top;margin:0px;padding-top:10px;padding-bottom:-900px;width:100px;"
              ),
              div(
                uiOutput("dimPlotUI2"), #%>% withLoader(proxy.height = proxy_height),
              ),
            style = "display: inline-block;"
            ),
            tags$style(HTML(".panel-group {margin-bottom:5px;margin-top:2px;}"))
          )
        ),
        tabPanel(
          introBox(
            span(icon("align-center", class = NULL, lib = "font-awesome"),
                 "Violin Plot",
                 title = "Plot expression or category data by category"
            ),
            data.step = 5,
            data.intro = "For a query gene, this tab displays the SCT normalized count expression violin plot.",
            data.position = "top"
          ),
          value = "Gene_violin",
          uiOutput("violinPlotUI1")
        ),
        tabPanel(
          introBox(
            span(icon("info", class = NULL, lib = "font-awesome"),
                 "About",
                 title = "View version and author info",
            ),
            value = "about",
            data.step = 6,
            data.intro = "Additional information on the study and authors.",
            data.position = "top"
          ),
          value = "about",
          h5("About the dataset"),
          uiOutput("GitHub"),
          # tags$a(
          #   href = manuscriptL,
          #   target="_blank",
          #   div(
          #     img(src = 'Placeholder.png', height = "100",
          #         alt = manuscriptT),
          #     style = "text-align: left;display: inline;"
          #   )
          # ),
          uiOutput("intro"),
          uiOutput("intro2"),
          
          uiOutput("rawdata"),
          uiOutput("version"),
          # hr(),
          # div(
          #   style = "display: inline-block;vertical-align:bottom;", 
          #   h5("Included Data Files")
          # ),
          # p("click to preview"),
          # fluidRow(
          #   column(width = 1),
          #   column(width = 9, DT::dataTableOutput("explain3"))
          # )
        )
      )
    )
  )
)
