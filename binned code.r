# 
# output$menuSubItems_species <- renderMenu({
# 
#   cSelSpec <- input$selectSpecs
# 
#   if(is.null(cSelSpec)){
#     species_menuSubList <- NULL
#   }else{
#     species_menuSubList <- lapply(cSelSpec, function(x){
#       specLabel <- gsub(" ", "_", x)
#       specTabId <- paste0("tab_SpecPars_", specLabel)
#       menuSubItem(x,  tabName = specTabId, icon = icon("sliders"))
#     })
#   }
#   sidebarMenu(.list = species_menuSubList)
# 
# })


tabItems(li(div(role="tabpanel", class = "tab-pane", id = paste0("shiny-tab-", "tab_testTab2"),
    h2("Test Tab")), 
div(role="tabpanel", class = "tab-pane", id = paste0("shiny-tab-", "tab_testTab2"),
    h2("Test Tab"))))
 
# tabItems(
#   tabItem(tabName="tab_species1Pars", class = "active",
#           h3("Species parameters Tab")),
#   tabItem(tabName="tab_turbWindPars",
#           h3("Turbine Parameters Tab")
#           ),
#   tabItem(tabName="tab_simulation",
#           h3("Simulation and Outputs Tab")
#   ),
#   tabItem(
#     tabName = "tab_testTab",
#     h2("Test Tab")
#   )
# )



# test <- data.frame(specName = species, specLabel = gsub(" ", "_", species))
# 
# test2 <- dlply(test, .(specLabel), function(data){
#   menuSubItem(data$specName,  tabName = paste0("tab_SpecPars_", data$specLabel), icon = icon("sliders"))
# })
# 
# 
# test2 <- sapply(test$specName, function(x){
#   specLabel <- gsub(" ", "_", x)
#   menuSubItem(x,  tabName = paste0("tab_SpecPars_", specLabel), icon = icon("sliders"))
# }, simplify = FALSE)
# 
# 
# sidebarMenu(.list=test2)





# # ---- tabItem builder for species-specific parameters
# output$tabItems_species <- renderUI({
#   
#   cSelSpec <- input$selectSpecs
#   specLabel <- gsub(" ", "_", cSelSpec)
#   
#   if(!is.null(cSelSpec)){
#     tabItem(
#       #tabName = paste0("tab_SpecPars_", specLabel[1]),
#       tabName = "testTab",
#       h2(paste0(cSelSpec[1]," biometric and Flight parameters"))
#     )
#   }
#   
# })


# fluidRow(
#   box(width = 12, title = "A Box in a Fluid Row I want to Split", 
#       splitLayout(
#         cellWidths = c("5%", "10%", "10%"),
#         p("January", style = "font-size: 12pt; font-weight: bold; margin-top: 40%;"),
#         numericInput("inputB", "Mean", value = NULL),
#         numericInput("inputC", "SD", value = 0)
#       ),
#       splitLayout(
#         cellWidths = c("5%", "10%", "10%"),
#         p("February", style = "font-size: 12pt; font-weight: bold; margin-top: 8%;"),
#         numericInput("inputD", label=NULL, value = 0),
#         numericInput("inputE", label=NULL, value = 0)
#       ),
#       splitLayout(
#         cellWidths = c("5%", "10%", "10%"),
#         p("March", style = "font-size: 12pt; font-weight: bold; margin-top: 8%;"),
#         numericInput("inputF", label=NULL, value = 0),
#         numericInput("inputG", label=NULL, value = 0)
#       ),
#       splitLayout(
#         cellWidths = c("5%", "10%", "10%"),
#         p("May", style = "font-size: 12pt; font-weight: bold; margin-top: 8%;"),
#         numericInput("inputH", label=NULL, value = 0),
#         numericInput("inputI", label=NULL, value = 0)
#       )
#   )
# ),









# # --- Store current input values of monthly densities of selected species
# inputs_monthDensPars <- reactive({
#   
#  # str(reactiveValuesToList(input))
#   RVs_inputs_ls <- reactiveValuesToList(input)
#   
#   RVs_DensParsInputs_ls <- RVs_inputs_ls[grep("birdDensPars", names(RVs_inputs_ls))]
#   
#   if(length(RVs_DensParsInputs_ls)>0){
#     
#     RVs_DensParsInputs_df <- RVs_DensParsInputs_ls %>%
#       ldply(function(x){data.frame(Value = as.character(x))}, .id = "inputTags") %>%
#       mutate(inputTags_split = str_split(inputTags, "_")) %>%
#       mutate(specLabel = map_chr(inputTags_split, function(x) paste(x[-c(1:4)], collapse = "_")),
#              par = paste0(map_chr(inputTags_split, function(x) x[2]), "_", map_chr(inputTags_split, function(x) x[3])),
#              hyper = map_chr(inputTags_split, function(x) x[4]),
#              par_hyper = paste(par, hyper, sep = "_")) %>%
#       arrange(specLabel, par_hyper)
#     
#     RVs_DensParsInputs_df
#   }
# })