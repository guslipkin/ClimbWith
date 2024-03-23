show_no_matches <- function() {
  shinyWidgets::show_alert(
    title = 'No Matches',
    text = 'No gyms match your filters',
    type = 'warning'
  )
}

show_about_us <- function() {
  shinyWidgets::show_alert(
    title = 'About Us',
    text = shiny::tagList(
      shiny::fluidPage(
        shiny::tags$h2('ClimbWith'),
        shiny::tags$p(
          "Gus started ClimbWith because he wanted to see where local gyms were
          located and what features they offered, beyond what's usually
          advertised. ClimbWith aims to provide the most accurate information on
          the features each gym offers such as climbing type and training
          boards, information about fitness equipment, and stats about wall
          heights.",
          style = 'font-size: .8em; text-align: justify;'
        ),
        shiny::tags$br(),
        shiny::tags$h2('The Team'),
        shiny::tags$h4(
          .get_tag_link('https://guslipkin.me', 'Gus Lipkin')
        ),
        shiny::fluidRow(
          shiny::column(
            width = 4,
            htmltools::img(
              src = 'www/images/gus_first_lead_comp.jpg', width = '80%'
            ),
            shiny::tags$p(shiny::tags$i(
              'Photo Credit:',
              .get_tag_link('https://www.instagram.com/shialabooya/', '@shialabooya'),
              style = 'font-size: .5em; text-align: center;'
            ))
          ),
          shiny::column(
            width = 8,
            shiny::tags$p(
              "Gus has been climbing since September 2023. He started out in the
              gym, first with bouldering, then top rope. In February 2024, Gus
              registered for a lead competition without any experience and spent
              the next three weeks frantically learning to lead. At the
              competition, he only completed one route and placed last in his
              category, but was happy to have participated. He occasionally
              posts videos of himself using tall person beta on his Instagram, ",
              .get_tag_link('https://www.instagram.com/gus_is_tall/', '@gus_is_tall'),
              ".",
              style = 'font-size: .7em; text-align: justify;'
            )
          )
        )
      )
    ),
    html = TRUE,
    btn_labels = 'Close'
  )
}
