#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(shiny)
library(tidyverse)

ui <- fluidPage(
    
    # Application title
    titlePanel("医院（安全）不良事件报告系统——ADR统计"),
    
    sidebarLayout(
        sidebarPanel(
            fileInput("FileOrigin", "请上传医院安全不良事件下载文件（勿做修改）",
                      multiple = FALSE,
                      accept = c( ".xls")),
            
            downloadButton("downloadtable", "Download"),
            downloadButton("downloadfigure", "Download")
        ),
        
        mainPanel(
            tabsetPanel(
                tabPanel("原数据", tableOutput("printFileOrigin")),
                tabPanel("统计结果", tableOutput("result")),
                tabPanel("数据校验", textOutput("printvalid")),
                tabPanel("柱形图", plotOutput("barplot"))
            )
        )
    )
)


server <- function(input, output) {
    
    temp <- reactive({
        
        infile <- input$FileOrigin
        readxl::read_excel(infile$datapath, skip = 7)
        
        
    })
    
    res <- reactive({
        ksmc_all <- tribble(
            ~`序号`, ~`科室名称`, ~`性质`,
            1,'儿科','非手术科室',
            2,'感染科','非手术科室',
            3,'中医科','非手术科室',
            4,'老年科','非手术科室',
            5,'肿瘤科','非手术科室',
            6,'皮肤科','非手术科室',
            7,'疼痛科','非手术科室',
            8,'内分泌科','非手术科室',
            9,'消化内科','非手术科室',
            10,'肾病内科','非手术科室',
            11,'神经内科','非手术科室',
            12,'全科医学科','非手术科室',
            13,'神经心理科','非手术科室',
            14,'心血管内科','非手术科室',
            15,'风湿免疫科','非手术科室',
            16,'血液内科','非手术科室',
            17,'重症医学科','非手术科室',
            18,'急诊医学科','非手术科室',
            19,'康复医学科','非手术科室',
            20,'临床营养科','非手术科室',
            21,'呼吸与危重症医学科','非手术科室',
            22,'眼科','手术科室',
            23,'骨外科','手术科室',
            24,'口腔科','手术科室',
            25,'妇科','手术科室',
            26,'产科','手术科室',
            27,'小儿外科','手术科室',
            28,'胸心外科','手术科室',
            29,'神经外科','手术科室',
            30,'泌尿外科','手术科室',
            31,'肝胆外一科','手术科室',
            32,'肝胆外二科','手术科室',
            33,'胃肠外一科','手术科室',
            34,'胃肠外二科','手术科室',
            35,'血管外科','手术科室',
            36,'口腔颌面外科','手术科室',
            37,'甲状腺乳腺外科','手术科室',
            38,'麻醉科、手术室','手术科室',
            39,'烧伤整形科','手术科室',
            40,'中西医结合肛肠科','手术科室',
            41,'耳鼻咽喉头颈外科','手术科室'
        )
        
        
        # 科室名称对照表
        ksmc <- data.frame(mc1 = c('ICU一病区', 'X急诊一病区', '儿科一病区', '风湿免疫一病区',
                                   '感染一病区','呼吸与危重症', '急诊一病区', '康复医学病区', 
                                   '老年一病区', '内分泌一病区', '皮肤一病区', '全科医学病区', 
                                   '神内二病区','神内一病区', '肾内一病区', '疼痛一病区', 
                                   '消内一病区','心内一病区', '新生儿一病区', '血液一病区', 
                                   '眼科一病区','中医一病区', '肿瘤一病区', '产科一病区', 
                                   '耳鼻喉一病区', '妇科一病区', '肝胆外二科病区', '肝胆外一科病区',
                                   '肛肠一病区', '骨外一病区', '颌外一病区', '甲状腺乳腺病区', 
                                   '泌外一病区', '烧伤一病区', '神外一病区', '胃肠外二病区', 
                                   '胃肠外一病区', '小儿外一病区', '胸外一病区', '血管外一病区',
                                   '心理卫生中心', '临床营养科', '肿瘤二病区'),
                           mc2 = c('重症医学科', '急诊医学科', '儿科', '风湿免疫科', '感染科', '呼吸与危重症医学科',
                                   '急诊医学科', '康复医学科','老年科', '内分泌科','皮肤科','全科医学科', 
                                   '神经内科','神经内科','肾病内科',  '疼痛科','消化内科', '心血管内科',
                                   '儿科','血液内科','眼科','中医科', '肿瘤科','产科','耳鼻咽喉头颈外科',
                                   '妇科','肝胆外二科','肝胆外一科', '中西医结合肛肠科', '骨外科','口腔颌面外科',
                                   '甲状腺乳腺外科','泌尿外科','烧伤整形科', '神经外科','胃肠外二科', '胃肠外一科', 
                                   '小儿外科', '胸心外科', '血管外科', '心理卫生中心', '临床营养科', '肿瘤科'))
        
        temp() %>% 
            filter(`科室名称` != "合计") %>% 
            mutate(`科室名称` = (str_replace_all(`科室名称`, c("医生站" = "", "护理单元" = "")))) %>% 
            left_join(ksmc, by = c("科室名称" = "mc1")) %>% 
            rename(`报告人科室` = mc2) %>% 
            mutate(`报告人科室` = if_else(`科室名称` == "精神科心理咨询", "神经心理科", `报告人科室`)) %>% 
            mutate(`报告人科室` = if_else(`科室名称` == "神内四病区", "神经内科", `报告人科室`)) %>% 
            mutate(`报告人科室` = if_else(`科室名称` == "心脏大血管外", "心脏大血管外科", `报告人科室`)) %>% 
            mutate(`报告人科室` = if_else(`科室名称` == "老年科", "老年科", `报告人科室`)) %>% 
            rename(`原科室名称` = `科室名称`) %>% 
            full_join(ksmc_all, by = c("报告人科室" = "科室名称")) %>% 
            mutate(`例数` = replace_na(`例数`, 0)) %>% 
            group_by(`报告人科室`) %>% 
            summarise(`例数` = sum(`例数`)) %>% 
            arrange(desc(`例数`)) %>%
            bind_rows(summarise(.,
                                across(where(is.numeric), sum),
                                across(where(is.character), ~"合计"))) %>% 
            mutate(`序号` = 1:n()) %>% 
            select(`序号`, `报告人科室`, `例数`)
        
    })
    
    output$result <- renderTable({
        
        res()
        
    })
    
    output$printFileOrigin <- renderTable({
        
        infile <- input$FileOrigin
        
        if(is.null(infile))
            return(NULL)
        
        readxl::read_excel(infile$datapath, skip = 7)
        
    })
    
    output$printvalid <- renderText({
        
        if(max(res()$`例数`) == max(temp()$`例数`)){
            print("总数校验无误！") 
        }else
            print("注意：不良事件总数有误，请核对！")
        
    })
    
    
    barplot <- function(){
        
        res() %>% 
            filter(`报告人科室` != "合计") %>% 
            mutate(`报告人科室` = factor(`报告人科室`, 
                                    levels = res()$`报告人科室`)) %>%
            ggplot(aes(`报告人科室`, `例数`)) + 
            geom_bar(stat = "identity", fill = "#87CEFA") +
            theme_bw() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
            xlab("")
        
    }
    
    output$barplot <- renderPlot({
        
        barplot()
        
    })
    
    
    output$downloadtable <- downloadHandler(
        
        filename = function() {
            "药品不良反应统计结果.xlsx"
        },
        content = function(file) {
            openxlsx::write.xlsx(res(), file, row.names = FALSE)
        }
        
    )
    
    
    output$downloadfigure <- downloadHandler(
        
        filename = "药品不良反应柱形图.png",
        content = function(file) {
            # device <- function(..., width, height) {
            #     grDevices::png(..., width = width, height = height,
            #                    res = 300, units = "in")
            # }
            ggsave(file, plot = barplot(), device = "png", width = 15, height = 10)
            
        })     
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)