# edna_app

This application's purpose is to assist in interpreting the evolution of environmental DNA (eDNA) concentration in streams. The application accepts two types of eDNA concentration data: total or by size. Which can be a function of distance or time. There are four main types of analysis that can be performed depending on the data available


1. **Clone the repository:**
    ```sh
    git clone https://github.com/dhallackla/edna_app.git
    cd edna_app
    ```

2. **Install the required packages:**
    ```r
    install.packages(c("shiny", "shinyWidgets", "tidyverse", "DescTools","optimx", "gt","broom", "shinyhelper","ggforce","markdown","ggnewscale","expint", "ggplot2")) 
    ```

3. **Run the app:**
    ```r
    library(shiny)
    runApp("path/to/your/app")
    ```
