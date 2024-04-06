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

show_terms <- function() {
  shinyWidgets::show_alert(
    title = 'Terms',
    text = shiny::tagList(
      shiny::fluidPage(
        shiny::fluidRow(
          shiny::column(
            width = 12,
            bs4Dash::box(
              title = shiny::tags$p('Terms of Service', style = 'font-size: 1.1em;'),
              width = 12,
              collapsible = TRUE,
              collapsed = TRUE,
              status = 'info',
              solidHeader = TRUE,
              shiny::tags$ol(
                shiny::tags$li(
                  "These terms also apply to the Data and Privacy policies."
                ),
                shiny::tags$li(
                  "Acceptance of Terms: By accessing or using this service, you
                  agree to be bound by these Terms of Service. If you do not
                  agree with any of these terms, you are prohibited from using
                  or accessing the service."
                ),
                shiny::tags$li(
                  "Service Availability: The author of this service reserves the
                  right to modify, suspend, or terminate the service at any time
                  without prior notice or liability. This includes the right to
                  terminate the service for users who abuse or misuse the
                  service, or attempt to cause harm by using the service."
                ),
                shiny::tags$li(
                  "User Conduct: Users are solely responsible for their conduct
                  while using the service, and are responsible for using the
                  service in a way that is respectful to the authors and other
                  users. Any violation of these terms may result in termination
                  of service without liability to the author."
                ),
                shiny::tags$li(
                  "Limitation of Liability: The authors of this service shall
                  not be liable for any direct, indirect, incidental, special,
                  consequential, or exemplary damages, including but not limited
                  to, damages for loss of profits, goodwill, use, data, or other
                  intangible losses resulting from the use or inability to use
                  the service. Users agree to indemnify and hold harmless the
                  authors of this service."
                ),
                shiny::tags$li(
                  "Changes to Terms: The author reserves the right to update or
                  modify these Terms of Service at any time without prior notice.
                  It is the user's responsibility to review these terms
                  periodically for any changes."
                ),
                shiny::tags$li(
                  "Governing Law: These Terms of Service shall be governed by
                  and construed in accordance with the laws of Massachusetts, US,
                  without regard to its conflict of law provisions."
                ),
                shiny::tags$li(
                  "Contact: If you have any questions or concerns about these
                  Terms of Service, there is a Contact link available on ",
                  .get_tag_link(.url(), 'ClimbWith'), "."
                ),
                shiny::tags$li(
                  "By using the this service, you acknowledge that you have
                  read, understood, and agree to be bound by these Terms of
                  Service."
                ),
              ),
              shiny::tags$p("Last updated: 2024-04-02"),
              style = 'font-size: .6em; text-align: justify;'
            )
          )
        ),
        shiny::fluidRow(
          shiny::column(
            width = 12,
            bs4Dash::box(
              title = shiny::tags$p('Data Policy', style = 'font-size: 1.1em;'),
              width = 12,
              collapsible = TRUE,
              collapsed = TRUE,
              status = 'info',
              solidHeader = TRUE,
              shiny::tags$ol(
                shiny::tags$li(
                  "User submitted data will be reviewed before being published
                  to ", .get_tag_link(.url(TRUE), 'ClimbWith'), '.'
                ),
                shiny::tags$li(
                  "Once cleaned, user submitted data will always be available to
                  users at ",
                  .get_tag_link(
                    "https://github.com/guslipkin/ClimbWith/blob/main/data-raw/data.csv",
                    'the project GitHub'
                  ), '.'
                ),
                shiny::tags$li(
                  "The link to the cleaned data will be kept up to date."
                ),
                shiny::tags$li(
                  "Because the data is user submitted, the team at ",
                  .get_tag_link(.url(), 'ClimbWith'),
                  " cannot guarantee that the data is accurate at all times."
                ),
                shiny::tags$li(
                  "The team at ", .get_tag_link(.url(), 'ClimbWith'),
                  " will do their best to validate that
                  the data available on ", .get_tag_link(.url(), 'ClimbWith'),
                  " is accurate and up to date and recommend calling a gym to
                  verify any information provided.",
                )
              ),
              shiny::tags$p("Last updated: 2024-04-02"),
              style = 'font-size: .6em; text-align: justify;'
            )
          )
        ),
        shiny::fluidRow(
          shiny::column(
            width = 12,
            bs4Dash::box(
              title = shiny::tags$p('Privacy Policy', style = 'font-size: 1.1em;'),
              width = 12,
              collapsible = TRUE,
              collapsed = TRUE,
              status = 'info',
              solidHeader = TRUE,
              shiny::tags$ol(
                shiny::tags$li(
                  "It is prohibited for users to submit data with personal
                  information attached for use with ",
                  .get_tag_link(.url(TRUE), 'ClimbWith'), '.'
                ),
                shiny::tags$li(
                  "Any submissions containing personal information will be
                  removed in their entirety."
                ),
                shiny::tags$li(
                  "The 'Locate Me' button works entirely in your browser.
                  No data is collected."
                )
              ),
              shiny::tags$p("Last updated: 2024-04-02"),
              style = 'font-size: .6em; text-align: justify;'
            )
          )
        )
      )
    ),
    html = TRUE,
    btn_labels = 'Close'
  )
}
