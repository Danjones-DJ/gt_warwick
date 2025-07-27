library(shiny)
library(tidyverse)
library(PostcodesioR)
library(geosphere)
library(conflicted)
conflict_prefer("span", "shiny")

# Load the new dataset
degree_data <- read.csv("https://raw.githubusercontent.com/Danjones-DJ/Degree-Matchmaker_DJ/refs/heads/main/gt_warwick.csv", 
                        stringsAsFactors = FALSE)

# Clean and prepare the data - UPDATE COLUMN NAMES
degree_data$median_salary <- as.numeric(degree_data$median_salary)

# Get dynamic degree types and universities from the actual data
degree_types <- sort(unique(degree_data$degree_type))
universities <- sort(unique(degree_data$university_name[!is.na(degree_data$university_name) & degree_data$university_name != ""]))

# OPTIMIZATION INFO: Check how many unique addresses vs total courses
unique_addresses_count <- length(unique(degree_data$provaddress[!is.na(degree_data$provaddress) & degree_data$provaddress != ""]))
total_courses_count <- nrow(degree_data)
cat("OPTIMIZATION: Only calculating", unique_addresses_count, "unique addresses instead of", total_courses_count, "courses\n")
cat("Speed improvement: ~", round(total_courses_count/unique_addresses_count, 1), "x faster\n")

# Load A-Level subjects from GitHub CSV
subjects_data <- read.csv("https://raw.githubusercontent.com/Danjones-DJ/Degree-Matchmaker_DJ/refs/heads/main/alevel_subjects.csv", 
                          stringsAsFactors = FALSE)
all_subjects <- sort(subjects_data$a_level_subjects)

btec_subjects <- c(
  "Applied Human Biology", "Applied Law", "Applied Psychology", "Applied Science", "Art and Design",
  "Business", "Children's Play, Learning and Development", "Computing", "Engineering",
  "Enterprise and Entrepreneurship", "Health and Social Care", "Information Technology",
  "Sport", "Sport and Exercise Science"
)
btec_grades <- c("D*", "D (Distinction)", "M (Merit)", "P (Pass)", "Fail")
# ...existing code...
all_subjects_combined <- c(all_subjects, btec_subjects)
all_grades_combined <- c("A*", "A", "B", "C", "D", "E", btec_grades)
# ...existing code...

# Subject synonym mapping for smart matching
create_subject_synonyms <- function() {
  list(
    "Mathematics" = c("Mathematics", "Maths", "Math", "Mathematical", "Further Mathematics", "Statistics"),
    "Physics" = c("Physics", "Physical", "Physical Sciences"),
    "Chemistry" = c("Chemistry", "Chemical", "Chemical Sciences"),
    "Biology" = c("Biology", "Biological", "Biological Sciences", "Life Sciences", "Life and Health Sciences"),
    "English Literature" = c("English Literature", "English", "Literature", "English Language and Literature"),
    "English Language" = c("English Language", "English", "Language", "English Language and Literature"),
    "History" = c("History", "Historical", "Ancient History"),
    "Geography" = c("Geography", "Geographical", "Environmental Geography"),
    "Computer Science" = c("Computer Science", "Computing", "ICT", "Information Technology", "Software Systems Development"),
    "Economics" = c("Economics", "Economic", "Business Economics"),
    "Psychology" = c("Psychology", "Psychological"),
    "Art and Design" = c("Art and Design", "Art", "Design", "Fine Art", "Visual Arts"),
    "Business" = c("Business", "Business Studies", "Commerce"),
    "French" = c("French", "French Language", "French Studies"),
    "German" = c("German", "German Language", "German Studies"),
    "Spanish" = c("Spanish", "Spanish Language", "Spanish Studies"),
    "Politics" = c("Politics", "Political Science", "Government", "Government and Politics"),
    "Philosophy" = c("Philosophy", "Philosophical"),
    "Sociology" = c("Sociology", "Social Sciences", "Sociological"),
    "Drama" = c("Drama", "Theatre", "Drama and Theatre", "Performing Arts"),
    "Music" = c("Music", "Musical", "Music Technology"),
    "Physical Education" = c("Physical Education", "PE", "Sports", "Sports Science"),
    "Religious Studies" = c("Religious Studies", "Religion", "Theology", "Islamic Studies", "Biblical Studies"),
    "Media Studies" = c("Media Studies", "Media", "Film Studies", "Digital Media and Design"),
    "Law" = c("Law", "Legal Studies", "Jurisprudence")
  )
}

# User's custom functions for distance calculation
extract_postcode <- function(provaddress) {
  address <- provaddress
  pattern <- "(?i)[A-Z]{1,2}[0-9]{1,2}[A-Z]?\\s[0-9][A-Z]{2}"
  postcode <- tolower(str_extract(address, pattern))
  return(postcode)
}

degree_dist <- function(post_code, provaddress) {
  tryCatch({
    home <- post_code
    dest <- extract_postcode(provaddress)
    
    if(is.na(home) || is.na(dest) || home == "" || dest == "") {
      return(data.frame(home = home, dest = dest, miles = NA, distance_range = "Unknown"))
    }
    
    df <- tibble(
      home = toupper(home),
      dest = toupper(dest)
    ) 
    
    df <- df %>%
      rowwise() %>%
      mutate(
        miles = distHaversine(
          postcode_lookup(home)[,7:8],
          postcode_lookup(dest)[,7:8]
        ) / 1609.34
      ) %>%
      ungroup()
    
    df <- df %>%
      mutate(          
        distance_range = cut(                                     
          miles,
          breaks = c(0, 10, 25, 50, 100, 150, Inf),
          labels = c("0‑10 miles", "10‑25 miles", "25‑50 miles", "50‑100 miles", "100‑150 miles", "150+ miles"),
          right  = FALSE                                
        )
      )
    
    return(df)
  }, error = function(e) {
    return(data.frame(home = post_code, dest = extract_postcode(provaddress), miles = NA, distance_range = "Error"))
  })
}

# Function to match subjects with requirements using synonyms
match_subjects_with_requirements <- function(selected_subjects, course_requirements) {
  if(is.null(selected_subjects) || length(selected_subjects) == 0 || 
     is.null(course_requirements) || is.na(course_requirements) || course_requirements == "") {
    return(FALSE)
  }
  
  synonyms <- create_subject_synonyms()
  
  # Create expanded list of all possible subject variations
  all_variations <- c()
  for(subject in selected_subjects) {
    if(subject %in% names(synonyms)) {
      all_variations <- c(all_variations, synonyms[[subject]])
    } else {
      all_variations <- c(all_variations, subject)
    }
  }
  
  # Check if any variation appears in the requirements
  any(sapply(all_variations, function(var) {
    grepl(var, course_requirements, ignore.case = TRUE)
  }))
}

# User's working matching function - UPDATED COLUMN NAME
find_matched_subjects <- function(a_levels, subject_requirements_example) {
  # Handle null/empty inputs
  if(is.null(a_levels) || length(a_levels) == 0 || 
     is.null(subject_requirements_example) || is.na(subject_requirements_example) || 
     subject_requirements_example == "") {
    return(character(0))
  }
  
  matched_subjects <- c()
  
  for (subject in a_levels) {
    if (grepl(subject, subject_requirements_example, ignore.case = TRUE)) {
      matched_subjects <- c(matched_subjects, subject)
      cat("match", subject, "\n")
    }
  }
  
  return(matched_subjects)
}

# Function to get student's selected subjects
get_selected_subjects <- function(input) {
  layout <- input$qual_layout
  if (is.null(layout) || layout == "a_levels") {
    subjects <- c(input$subject1, input$subject2, input$subject3, input$subject4)
    subjects <- subjects[!is.null(subjects) & subjects != ""]
    return(subjects)
  } else if (layout == "mix") {
    subjects <- c()
    for (i in 1:4) {
      subj <- input[[paste0("subject", i)]]
      if (!is.null(subj) && subj != "") {
        subjects <- c(subjects, subj)
      }
    }
    return(subjects)
  }
  return(character(0))
}

# Function to convert grade requirements to scores - FIXED A* PARSING
convert_grade_requirement <- function(grade_req) {
  if(is.na(grade_req) || grade_req == "") return(0)
  
  # Handle A* grades more carefully
  # Split by A* first, then handle remaining
  parts <- strsplit(grade_req, "A\\*")[[1]]
  
  # Count A* grades
  num_a_star <- length(parts) - 1
  
  # Get remaining grades from the last part
  remaining <- parts[length(parts)]
  remaining_grades <- if(remaining == "") character(0) else unlist(strsplit(remaining, ""))
  remaining_grades <- remaining_grades[remaining_grades %in% c("A", "B", "C", "D", "E", "U")]
  
  # Combine all grades
  all_grades <- c(rep("A*", num_a_star), remaining_grades)
  
  if(length(all_grades) == 0) return(0)
  
  # Calculate total score
  scores <- sapply(all_grades, grade_to_score, USE.NAMES = FALSE)
  return(sum(scores))
}

# Simple similar courses finder - UPDATED COLUMN NAMES
find_similar_courses <- function(current_course, all_courses, limit = 3) {
  all_courses <- all_courses[all_courses$title != current_course$title, ]
  if (nrow(all_courses) == 0) return(all_courses[0, ])
  
  # Helper: extract all 1-3 word phrases (and single words > 4 chars)
  extract_phrases <- function(title) {
    words <- unlist(strsplit(title, "\\s+"))
    n <- length(words)
    phrases <- c()
    # 1-word (if long enough)
    phrases <- c(phrases, words[nchar(words) > 4])
    # 2-word
    if (n >= 2) {
      for (i in 1:(n-1)) phrases <- c(phrases, paste(words[i], words[i+1]))
    }
    # 3-word
    if (n >= 3) {
      for (i in 1:(n-2)) phrases <- c(phrases, paste(words[i], words[i+1], words[i+2]))
    }
    unique(phrases)
  }
  # Lowercase, remove generic
  generic_words <- c("science", "sciences", "studies", "study", "with", "and", "the", "of", "in")
  current_phrases <- tolower(extract_phrases(current_course$title))
  current_phrases <- current_phrases[!current_phrases %in% generic_words]
  
  # Flexible phrase/word match (across ALL degrees)
  direct_phrase_matches <- all_courses[
    sapply(all_courses$title, function(other_title) {
      other_title_lower <- tolower(other_title)
      other_phrases <- tolower(extract_phrases(other_title))
      other_phrases <- other_phrases[!other_phrases %in% generic_words]
      # Match if any phrase from current in other, or vice versa
      any(sapply(current_phrases, function(phrase) grepl(phrase, other_title_lower, fixed = TRUE))) ||
        any(sapply(other_phrases, function(phrase) grepl(phrase, tolower(current_course$title), fixed = TRUE)))
    }),
  ]
  direct_phrase_matches <- direct_phrase_matches[!duplicated(direct_phrase_matches$title), ]
  
  # If enough, return
  if (nrow(direct_phrase_matches) >= limit) return(head(direct_phrase_matches, limit))
  
  # 2. Interest category overlap (independent, not stacked)
  all_interest_categories <- c(
    "Natural Sciences", "Humanities", "Architecture", "Computational & Mathematical Sciences",
    "Social Sciences", "Management", "Medicine", "Sustainability", "Engineering",
    "Languages", "Arts", "Education", "Technology", "Law"
  )
  interest_groups <- c()
  for (cat in all_interest_categories) {
    cat_courses <- get_subject_category_courses(cat, all_courses)
    if (current_course$title %in% cat_courses$title) {
      interest_groups <- c(interest_groups, cat)
    }
  }
  # Always use the same columns as all_courses
  interest_matches <- all_courses[0, ]
  for (cat in interest_groups) {
    interest_matches <- rbind(interest_matches, get_subject_category_courses(cat, all_courses))
  }
  interest_matches <- interest_matches[!duplicated(interest_matches$title), ]
  # Remove any already matched
  interest_matches <- interest_matches[!interest_matches$title %in% direct_phrase_matches$title, ]
  
  # Combine, up to limit
  all_matches <- rbind(direct_phrase_matches, interest_matches)
  all_matches <- all_matches[!duplicated(all_matches$title), ]
  if (nrow(all_matches) >= limit) return(head(all_matches, limit))
  
  # 3. Fallback: fill with any remaining courses, preferring shorter titles
  filler <- all_courses[!all_courses$title %in% all_matches$title, ]
  if (nrow(filler) > 0) {
    filler$title_word_count <- sapply(strsplit(filler$title, "\\s+"), length)
    filler <- filler[order(filler$title_word_count, filler$title), ]
    all_matches <- rbind(all_matches, head(filler, limit - nrow(all_matches)))
    filler$title_word_count <- NULL
  }
  
  return(head(all_matches, limit))
}

# Function to determine match type - FIXED LOGIC
get_match_type <- function(student_score, course_score) {
  if(is.na(student_score) || is.na(course_score)) return("No Data")
  
  if(student_score == course_score) return("Exact Match")
  else if(student_score > course_score) {
    difference <- student_score - course_score
    if(difference <= 30) return("Good Match")  # Within ~1 grade difference
    else return("Overmatch")
  }
  else return("Under Match")  # student_score < course_score means underqualified
}
grade_to_score <- function(grade) {
  # A Level grades
  if (grade == "A*") return(56)
  if (grade == "A")  return(48)
  if (grade == "B")  return(40)
  if (grade == "C")  return(32)
  if (grade == "D")  return(24)
  if (grade == "E")  return(16)
  # BTEC grades (Extended Certificate UCAS points)
  if (grade == "D*") return(56)
  if (grade == "D (Distinction)")  return(48)
  if (grade == "M (Merit)")  return(32)
  if (grade == "P (Pass)")  return(16)
  if (grade == "Fail") return(0)
  return(0)
}

# Function to calculate student's best 3 grades score  
calculate_student_score <- function(grades) {
  # Remove empty grades
  valid_grades <- grades[grades != "" & !is.na(grades)]
  
  # Must have at least 3 grades
  if(length(valid_grades) < 3) return(NA)
  
  # Convert to scores and take best 3
  scores <- sapply(valid_grades, grade_to_score, USE.NAMES = FALSE)
  best_3_scores <- sort(scores, decreasing = TRUE)[1:3]
  
  return(sum(best_3_scores))
}

# --- IB score calculation helpers ---
calculate_ib_score <- function(grades, bonus) {
  grades_num <- suppressWarnings(as.numeric(grades))
  grades_num <- grades_num[!is.na(grades_num)]
  if(length(grades_num) < 6) return(NA)
  total <- sum(grades_num[1:6]) + as.numeric(bonus)
  return(total)
}

get_ib_grades <- function(input) {
  grades <- c(input$ib_g1_grade, input$ib_g2_grade, input$ib_g3_grade, input$ib_g4_grade, input$ib_g5_grade, input$ib_g6_grade)
  bonus <- ifelse(is.null(input$ib_core_bonus), 0, as.numeric(input$ib_core_bonus))
  list(grades = grades, bonus = bonus)
}

# Add grade scores to dataset - UPDATED COLUMN NAME
degree_data$grade_score <- sapply(degree_data$a_level_grade_req, convert_grade_requirement)

# Sort dataset by grade requirement (highest first)
degree_data <- degree_data[order(-degree_data$grade_score, na.last = TRUE), ]

# Subject categories mapping (simplified for now - you can expand this based on your new dataset)
get_subject_category_courses <- function(category, data) {
  if(category == "Natural Sciences") {
    keywords <- c("Applied Medical Sciences", "Audiology", "Biochemistry", "Biological Sciences", 
                  "Biomedical Sciences", "Bioprocessing", "Business and Health", "Cancer Biomedicine",
                  "Chemistry", "Earth Sciences", "Environmental Geoscience", "Geography and Economics",
                  "Geography", "Geology", "Human Neuroscience", "Human Sciences", "Infection and Immunity",
                  "Mathematics with Mathematical Physics", "Mathematics and Physics", "Neuroscience",
                  "Nutrition and Medical Sciences", "Population Health Sciences", "Psychology",
                  "Science and Engineering", "Sport and Exercise Medical Sciences", "Sustainable Built",
                  "Theoretical Physics", "Biochemical Engineering", "Biomedical Engineering")
    pattern <- paste(keywords, collapse = "|")
    return(data[grepl(pattern, data$title, ignore.case = TRUE), ])
    
  } else if(category == "Humanities") {
    keywords <- c("Anthropology", "Archaeology", "Experimental Linguistics", "Global Humanitarian",
                  "History and Philosophy", "Philosophy, Politics and Economics", "Politics and International",
                  "Urban Planning", "Urban Studies", "Ancient History", "Classical", "Classics",
                  "Comparative Literature", "Creative Arts and Humanities", "Education, Society",
                  "History", "Philosophy", "Politics, Sociology", "Viking", "Bulgarian", "Czech",
                  "Finnish", "Hungarian", "Polish", "Romanian", "Russian and History", "Ukrainian", "Serbian", "Croatian")
    pattern <- paste(keywords, collapse = "|")
    return(data[grepl(pattern, data$title, ignore.case = TRUE), ])
    
  } else if(category == "Architecture") {
    keywords <- c("Architectural", "Architecture")
    pattern <- paste(keywords, collapse = "|")
    return(data[grepl(pattern, data$title, ignore.case = TRUE), ])
    
  } else if(category == "Computational & Mathematical Sciences") {
    keywords <- c("Astrophysics", "Computer Science", "Crime and Security Science", "Data Science",
                  "Geophysics", "Mathematics", "Statistical Science", "Statistics", "Physics",
                  "Electronic and Electrical Engineering", "Mechanical Engineering", "Philosophy and Computer")
    pattern <- paste(keywords, collapse = "|")
    return(data[grepl(pattern, data$title, ignore.case = TRUE), ])
    
  } else if(category == "Social Sciences") {
    keywords <- c("Social Sciences", "Geography", "Economics", "Politics", "Sociology", "European Social")
    pattern <- paste(keywords, collapse = "|")
    return(data[grepl(pattern, data$title, ignore.case = TRUE), ])
    
  } else if(category == "Management") {
    keywords <- c("Management", "Business")
    pattern <- paste(keywords, collapse = "|")
    return(data[grepl(pattern, data$title, ignore.case = TRUE), ])
    
  } else if(category == "Medicine") {
    keywords <- c("Medical", "Medicine", "Biomedical", "Cancer", "Neuroscience", "Pharmacology", "Sport and Exercise")
    pattern <- paste(keywords, collapse = "|")
    return(data[grepl(pattern, data$title, ignore.case = TRUE), ])
    
  } else if(category == "Sustainability") {
    keywords <- c("Sustainable", "Sustainability")
    pattern <- paste(keywords, collapse = "|")
    return(data[grepl(pattern, data$title, ignore.case = TRUE), ])
    
  } else if(category == "Engineering") {
    keywords <- c("Engineering", "Computer Science")
    pattern <- paste(keywords, collapse = "|")
    return(data[grepl(pattern, data$title, ignore.case = TRUE), ])
    
  } else if(category == "Languages") {
    languages <- c("Dutch", "French", "German", "Hebrew", "Hungarian", "Italian", "Norwegian", 
                   "Polish", "Romanian", "Russian Studies", "Scandinavian Studies", 
                   "Spanish and Latin American Studies", "Bulgarian", "Czech", "Danish", 
                   "Finnish", "Serbian", "Croatian", "Swedish", "Ukrainian", "Ancient Languages",
                   "Linguistics", "Psychology and Language Sciences")
    pattern <- paste(languages, collapse = "|")
    return(data[grepl(pattern, data$title, ignore.case = TRUE), ])
    
  } else if(category == "Arts") {
    keywords <- c("Fine Art", "Art", "History of Art", "Media", "Creative Arts", "English", "Literature")
    pattern <- paste(keywords, collapse = "|")
    return(data[grepl(pattern, data$title, ignore.case = TRUE), ])
    
  } else if(category == "Education") {
    keywords <- c("Education", "Early Childhood")
    pattern <- paste(keywords, collapse = "|")
    return(data[grepl(pattern, data$title, ignore.case = TRUE), ])
    
  } else if(category == "Technology") {
    keywords <- c("Information Management", "Art and Technology", "Electronic and Electrical")
    pattern <- paste(keywords, collapse = "|")
    return(data[grepl(pattern, data$title, ignore.case = TRUE), ])
    
  } else if(category == "Law") {
    keywords <- c("Law", "Laws")
    pattern <- paste(keywords, collapse = "|")
    return(data[grepl(pattern, data$title, ignore.case = TRUE), ])
  }
  
  return(data)
}

# UI with Spotify-inspired design BUT ALL FUNCTIONALITY + POSTCODE INPUT
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Circular:wght@300;400;500;600;700;800&display=swap');
      
      * {
        box-sizing: border-box;
        margin: 0;
        padding: 0;
      }
      
      body, html {
        background: linear-gradient(135deg, #191414 0%, #1a1a1a 50%, #121212 100%) !important;
        color: #ffffff !important;
        font-family: 'Circular', -apple-system, BlinkMacSystemFont, 'Segoe UI', 'Roboto', sans-serif !important;
        overflow-x: hidden;
        min-height: 100vh;
      }
      
      .container-fluid {
        padding: 0 !important;
        margin: 0 !important;
        background: transparent !important;
      }
      
      /* SPOTIFY-STYLE HEADER */
      .spotify-header {
        background: linear-gradient(135deg, #FF6B35 0%, #F7931E 50%, #E85A4F 100%) !important;
        padding: 40px 60px;
        color: #fff !important;
        position: relative;
        overflow: hidden;
      }
      
      .spotify-header::before {
        content: '';
        position: absolute;
        top: 0;
        left: 0;
        right: 0;
        bottom: 0;
        background: url('data:image/svg+xml,<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 100 100\"><circle cx=\"20\" cy=\"20\" r=\"2\" fill=\"%23ffffff\" opacity=\"0.1\"/><circle cx=\"80\" cy=\"40\" r=\"1\" fill=\"%23ffffff\" opacity=\"0.15\"/><circle cx=\"40\" cy=\"80\" r=\"1.5\" fill=\"%23ffffff\" opacity=\"0.1\"/></svg>');
        background-size: 200px 200px;
        animation: float 20s ease-in-out infinite;
      }
      
      @keyframes float {
        0%, 100% { transform: translateY(0px) translateX(0px); }
        50% { transform: translateY(-10px) translateX(5px); }
      }
      
      .header-content {
        position: relative;
        z-index: 2;
      }
      
      .main-title {
        font-size: 48px;
        font-weight: 800;
        margin-bottom: 8px;
        color: #000;
        text-shadow: 0 2px 4px rgba(0,0,0,0.1);
      }
      .badge-alevel {
        background: linear-gradient(90deg, #4b90e2 0%, #2196F3 100%) !important;
        color: #fff !important;
        border-radius: 20px;
        padding: 4px 14px;
        font-weight: 700;
        font-size: 13px;
        margin-left: 8px;
      }
      .badge-ib {
        background: linear-gradient(90deg, #FF9800 0%, #FF6B35 100%) !important;
        color: #fff !important;
        border-radius: 20px;
        padding: 4px 14px;
        font-weight: 700;
        font-size: 13px;
        margin-left: 8px;
      }
      
      .main-subtitle {
        font-size: 18px;
        font-weight: 400;
        color: rgba(0,0,0,0.7);
        margin-bottom: 0;
      }
      
      /* MAIN CONTENT AREA - FULL WIDTH */
      .main-content {
        padding: 40px 60px;
        max-width: 100% !important;
        width: 100% !important;
        margin: 0 auto;
      }
      
      /* SPOTIFY-STYLE SECTION HEADERS */
      .section-header {
        font-size: 32px;
        font-weight: 700;
        color: #ffffff;
        margin: 60px 0 30px 0;
        letter-spacing: -0.5px;
      }
      
      .section-subtitle {
        font-size: 16px;
        font-weight: 400;
        color: #b3b3b3;
        margin-bottom: 30px;
        line-height: 1.5;
      }
      
      /* CARD-BASED LAYOUT */
      .card-container {
        background: linear-gradient(135deg, #1a1a1a 0%, #2a2a2a 100%);
        border-radius: 16px;
        padding: 32px;
        margin-bottom: 32px;
        box-shadow: 0 8px 32px rgba(0,0,0,0.3);
        transition: all 0.3s cubic-bezier(0.25, 0.46, 0.45, 0.94);
        border: 1px solid rgba(255,255,255,0.05);
      }
      
      .card-container:hover {
        transform: translateY(-4px);
        box-shadow: 0 16px 48px rgba(0,0,0,0.4);
        border-color: rgba(255, 107, 53, 0.3) !important;
      }
      
      /* GRADES & SUBJECTS SECTION - FORCED 4 COLUMNS */
      .grades-grid {
        display: grid !important;
        grid-template-columns: repeat(4, 1fr) !important; /* Default: 4 columns */
        gap: 12px !important;
        margin-bottom: 12px !important;
        width: 100%;
        max-width: auto;
        margin-left: auto;
        margin-right: auto;
      }

      .grades-grid-btec {
        display: grid !important;
        grid-template-columns: repeat(3, 1fr) !important; /* 3 columns for mix */
        gap: 12px !important;
        margin-bottom: 12px !important;
        width: 100%;
        max-width: auto;
        margin-left: auto;
        margin-right: auto;
      }

     /* Shrink the subject-grade-pair boxes */
      .subject-grade-pair {
        background: rgba(255,255,255,0.02);
        border-radius: 8px;
        padding: 8px 6px;
        border: 1px solid rgba(255,255,255,0.04);
        margin-bottom: 0;
      }

      /* Shrink the label text */
      .field-label {
        font-size: 10px;
        font-weight: 600;
        color: #FF6B35 !important;
        text-transform: uppercase;
        letter-spacing: 0.5px;
        margin-bottom: 4px;
      }

      /* Shrink the select inputs */
      .subject-grade-pair select,
      .subject-grade-pair .selectize-input {
        font-size: 12px !important;
        padding: 6px 10px !important;
        border-radius: 6px !important;
        min-height: 28px !important;
      }

      /* Responsive: stack vertically on mobile */
      @media (max-width: 900px) {
        .grades-grid {
            grid-template-columns: 1fr !important;
        }
      }


      
      @media (max-width: 768px) {
        .grades-grid {
          grid-template-columns: repeat(2, 1fr) !important;
        }
      }
      
      @media (max-width: 480px) {
        .grades-grid {
          grid-template-columns: 1fr !important;
        }
      }
      
      
      .subject-grade-pair select {
        width: 100%;
        background: rgba(255,255,255,0.08) !important;
        border: 1px solid rgba(255,255,255,0.1) !important;
        border-radius: 8px !important;
        padding: 12px 16px !important;
        color: #ffffff !important;
        font-size: 14px !important;
        font-weight: 500 !important;
        transition: all 0.3s ease !important;
        outline: none !important;
      }
      
      .subject-grade-pair select:focus {
        border-color: #FF6B35 !important;
        background: rgba(255, 107, 53, 0.1) !important;
        box-shadow: 0 0 0 3px rgba(255, 107, 53, 0.2) !important;
      }
      
      .subject-grade-pair select option {
        background: #2a2a2a !important;
        color: #ffffff !important;
        padding: 8px !important;
      }
      
      /* POSTCODE INPUT STYLING */
      .postcode-container {
        background: rgba(255,255,255,0.03);
        border-radius: 12px;
        padding: 20px;
        transition: all 0.3s ease;
        border: 1px solid rgba(255,255,255,0.05);
        max-width: 300px;
        margin: 0 auto;
      }
      
      .postcode-container:hover {
        background: rgba(255,255,255,0.06);
        border-color: rgba(255, 107, 53, 0.4) !important;
        transform: translateY(-2px);
      }
      
      .postcode-container input {
        width: 100%;
        background: rgba(255,255,255,0.08) !important;
        border: 1px solid rgba(255,255,255,0.1) !important;
        border-radius: 8px !important;
        padding: 12px 16px !important;
        color: #ffffff !important;
        font-size: 14px !important;
        font-weight: 500 !important;
        transition: all 0.3s ease !important;
        outline: none !important;
        text-align: center;
        text-transform: uppercase;
      }
      
      .postcode-container input:focus {
        border-color: #FF6B35 !important;
        background: rgba(255, 107, 53, 0.1) !important;
        box-shadow: 0 0 0 3px rgba(255, 107, 53, 0.2) !important;
      }
      
      .postcode-container input::placeholder {
        color: #888888 !important;
        text-transform: none;
      }
      
      /* INTERESTS SECTION */
      .interests-grid {
        display: grid;
        grid-template-columns: repeat(auto-fill, minmax(200px, 1fr));
        gap: 16px;
      }
      
      .interest-card {
        background: rgba(255,255,255,0.03);
        border: 1px solid rgba(255,255,255,0.1);
        border-radius: 12px;
        padding: 20px 16px;
        text-align: center;
        font-size: 14px;
        font-weight: 500;
        color: #ffffff;
        cursor: pointer;
        transition: all 0.3s cubic-bezier(0.25, 0.46, 0.45, 0.94);
        user-select: none;
        min-height: 80px;
        display: flex;
        align-items: center;
        justify-content: center;
        text-align: center;
        line-height: 1.3;
      }
      
      .interest-card:hover {
        background: rgba(255,255,255,0.08);
        border-color: rgba(255, 107, 53, 0.5) !important;
        transform: translateY(-3px) scale(1.02);
        box-shadow: 0 8px 25px rgba(0,0,0,0.3);
      }
      
      .interest-card.selected {
        background: linear-gradient(135deg, #FF6B35 0%, #E85A4F 100%) !important;
        border-color: #FF6B35 !important;
        color: #ffffff !important;
        font-weight: 600 !important;
        transform: translateY(-3px) scale(1.02);
        box-shadow: 0 8px 25px rgba(255, 107, 53, 0.4);
      }
      
      /* DEGREE TYPES */
      .degree-grid {
        display: grid;
        grid-template-columns: repeat(auto-fill, minmax(120px, 1fr));
        gap: 12px;
      }
      
      .degree-card {
        background: rgba(255,255,255,0.03);
        border: 1px solid rgba(255,255,255,0.1);
        border-radius: 8px;
        padding: 16px 12px;
        text-align: center;
        font-size: 13px;
        font-weight: 500;
        color: #ffffff;
        cursor: pointer;
        transition: all 0.3s ease;
        user-select: none;
        min-height: 60px;
        display: flex;
        align-items: center;
        justify-content: center;
      }
      
      .degree-card:hover {
        background: rgba(255,255,255,0.08);
        border-color: rgba(255, 107, 53, 0.5) !important;
        transform: translateY(-2px);
      }
      
      .degree-card.selected {
        background: linear-gradient(135deg, #FF6B35 0%, #E85A4F 100%) !important;
        border-color: #FF6B35 !important;
        color: #ffffff !important;
        font-weight: 600 !important;
      }
      
      .degree-card.small-text {
        font-size: 11px;
        padding: 12px 8px;
      }
      
      /* DISTANCE FILTER SECTION */
      .distance-grid {
        display: grid;
        grid-template-columns: repeat(auto-fill, minmax(140px, 1fr));
        gap: 12px;
      }
      
      .distance-card {
        background: rgba(255,255,255,0.03);
        border: 1px solid rgba(255,255,255,0.1);
        border-radius: 8px;
        padding: 16px 12px;
        text-align: center;
        font-size: 13px;
        font-weight: 500;
        color: #ffffff;
        cursor: pointer;
        transition: all 0.3s ease;
        user-select: none;
        min-height: 60px;
        display: flex;
        align-items: center;
        justify-content: center;
      }
      
      .distance-card:hover {
        background: rgba(255,255,255,0.08);
        border-color: rgba(255, 107, 53, 0.5) !important;
        transform: translateY(-2px);
      }
      
      .distance-card.selected {
        background: linear-gradient(135deg, #FF6B35 0%, #E85A4F 100%) !important;
        border-color: #FF6B35 !important;
        color: #ffffff !important;
        font-weight: 600 !important;
      }
      
      /* SPOTIFY-STYLE BUTTON */
      .spotify-btn {
        background: linear-gradient(135deg, #FF6B35 0%, #E85A4F 100%) !important;
        border: none;
        border-radius: 50px;
        padding: 16px 48px;
        color: #fff !important;
        font-size: 16px;
        font-weight: 700;
        text-transform: uppercase;
        letter-spacing: 1px;
        cursor: pointer;
        transition: all 0.3s cubic-bezier(0.25, 0.46, 0.45, 0.94);
        box-shadow: 0 8px 24px rgba(255, 107, 53, 0.3);
        margin: 40px auto;
        display: block;
        position: relative;
        overflow: hidden;
      }
      
      .spotify-btn:hover {
        transform: translateY(-3px) scale(1.05);
        box-shadow: 0 12px 36px rgba(255, 107, 53, 0.4);
        background: linear-gradient(135deg, #FF8A50 0%, #F76B47 100%) !important;
      }
      
      .spotify-btn:active {
        transform: translateY(-1px) scale(1.02);
      }
      
      /* FILTER TABS */
      .filter-section {
        margin: 40px 0;
        padding: 0;
      }
      
      .filter-tabs {
        display: flex;
        gap: 8px;
        margin-bottom: 30px;
        flex-wrap: wrap;
      }
      
      .filter-tab {
        background: rgba(255,255,255,0.05);
        border: 1px solid rgba(255,255,255,0.1);
        border-radius: 50px;
        padding: 12px 24px;
        color: #b3b3b3;
        font-size: 14px;
        font-weight: 500;
        cursor: pointer;
        transition: all 0.3s ease;
        text-decoration: none;
      }
      
      .filter-tab:hover {
        background: rgba(255,255,255,0.08);
        color: #ffffff;
        border-color: rgba(255,255,255,0.2);
      }
      
      .filter-tab.active {
        background: #ffffff;
        color: #000000;
        border-color: #ffffff;
        font-weight: 600;
      }
      
      .filter-controls {
        display: flex;
        gap: 20px;
        align-items: center;
        justify-content: flex-end;
        flex-wrap: wrap;
      }
      
      .filter-controls select {
        background: rgba(255,255,255,0.05) !important;
        border: 1px solid rgba(255,255,255,0.1) !important;
        border-radius: 8px !important;
        padding: 8px 12px !important;
        color: #ffffff !important;
        font-size: 14px !important;
        min-width: 120px;
      }
      
      .filter-controls select:focus {
        border-color: #4b90e2 !important;
        outline: none !important;
      }
      
      .filter-controls label {
        color: #b3b3b3;
        font-size: 14px;
        font-weight: 500;
      }
      
      /* COURSE CARDS - FORCED 2 COLUMNS LAYOUT */
      .courses-grid {
        display: grid !important;
        grid-template-columns: repeat(2, minmax(0, 1fr)) !important;
        gap: 30px !important;
        margin-top: 30px !important;
        width: 100% !important;
        padding: 0 10px !important;
        box-sizing: border-box !important;
      }
      
      .courses-grid > * {
        display: contents !important;
      }
      
      .course-card {
        background: linear-gradient(135deg, #1a1a1a 0%, #2a2a2a 100%);
        border-radius: 12px;
        padding: 20px;
        border: 1px solid rgba(255,255,255,0.05);
        transition: all 0.3s ease;
        cursor: pointer;
        position: relative;
        overflow: hidden;
        min-height: 180px;
        width: 100% !important;
        box-sizing: border-box !important;
        margin: 0 !important;
      }
      
      .course-card::before {
        content: '';
        position: absolute;
        top: 0;
        left: 0;
        right: 0;
        height: 4px;
        background: linear-gradient(90deg, #FF6B35 0%, #E85A4F 100%) !important;
        transform: scaleX(0);
        transition: transform 0.3s ease;
        transform-origin: left;
      }
      
      .course-card:hover {
        transform: translateY(-8px);
        box-shadow: 0 16px 48px rgba(0,0,0,0.4);
        border-color: rgba(255, 107, 53, 0.3) !important;
      }
      
      .course-card:hover::before {
        transform: scaleX(1);
      }
      
      /* Responsive breakpoints */
      @media (min-width: 1200px) {
        .courses-grid {
          grid-template-columns: repeat(2, minmax(0, 1fr)) !important;
          padding: 0 20px !important;
        }
      }
      
      @media (max-width: 768px) {
        .courses-grid {
          grid-template-columns: 1fr !important;
          padding: 0 10px !important;
        }
      }
      
      .match-badge {
        display: inline-block;
        padding: 4px 12px;
        border-radius: 20px;
        font-size: 11px;
        font-weight: 600;
        text-transform: uppercase;
        letter-spacing: 0.5px;
        margin-bottom: 12px;
      }
      
      .match-exact { background: #4b90e2; color: #000; }
      .match-good { background: #2196F3; color: #fff; }
      .match-over { background: #FF9800; color: #000; }
      .match-no { background: #f44336; color: #fff; }
      
      .course-title {
        font-size: 20px;
        font-weight: 700;
        color: #ffffff;
        margin-bottom: 8px;
        line-height: 1.3;
        cursor: pointer;
        transition: color 0.3s ease;
      }
      
      .course-title:hover {
        color: #FF6B35;
      }
      
      .course-details {
        font-size: 14px;
        color: #b3b3b3;
        margin-bottom: 16px;
        line-height: 1.4;
      }
      
      .course-actions {
        display: flex;
        gap: 12px;
      }
      
      .course-btn {
        background: rgba(255,255,255,0.05);
        border: 1px solid rgba(255,255,255,0.1);
        border-radius: 20px;
        padding: 8px 16px;
        color: #ffffff;
        font-size: 12px;
        font-weight: 500;
        cursor: pointer;
        transition: all 0.3s ease;
        text-decoration: none;
      }
      
      .course-btn:hover {
        background: #FF6B35;
        color: #000000;
        border-color: #FF6B35;
        transform: translateY(-2px);
      }
      
      .course-btn.primary {
        background: #FF6B35;
        color: #000000;
        border-color: #FF6B35;
      }
      
      .course-btn.primary:hover {
        background: #FF8A50;
        transform: translateY(-2px) scale(1.05);
      }
      
      /* Distance badge styling */
      .distance-badge {
        display: inline-block;
        padding: 4px 8px;
        border-radius: 12px;
        font-size: 10px;
        font-weight: 600;
        background: rgba(255, 107, 53, 0.2);
        color: #FF6B35;
        margin-left: 8px;
        border: 1px solid rgba(255, 107, 53, 0.3);
      }
      
      /* MODAL STYLING */
      .custom-modal {
        display: none;
        position: fixed;
        z-index: 1000;
        left: 0;
        top: 0;
        width: 100%;
        height: 100%;
        background: rgba(0,0,0,0.9);
        backdrop-filter: blur(10px);
        padding: 20px;
        box-sizing: border-box;
      }
      
      .modal-content-custom {
        background: linear-gradient(135deg, #191414 0%, #1a1a1a 100%);
        border-radius: 16px;
        max-width: 1200px;
        margin: 0 auto;
        padding: 40px;
        color: white;
        position: relative;
        max-height: 90vh;
        overflow-y: auto;
        box-shadow: 0 25px 50px rgba(0,0,0,0.8);
        border: 1px solid rgba(255,255,255,0.1);
      }
      
      .modal-close {
        position: absolute;
        top: 20px;
        right: 24px;
        font-size: 24px;
        font-weight: bold;
        cursor: pointer;
        color: #b3b3b3;
        transition: color 0.3s ease;
      }
      
      .modal-close:hover {
        color: #4b90e2;
      }
      
      .go-back-btn {
        background: linear-gradient(135deg, #4b90e2 0%, #3a7bc8 100%);
        border: none;
        border-radius: 50px;
        padding: 12px 24px;
        color: #000;
        margin-bottom: 25px;
        cursor: pointer;
        font-weight: 600;
        transition: all 0.3s ease;
        font-size: 14px;
      }
      
      .go-back-btn:hover {
        transform: translateY(-2px) scale(1.05);
        box-shadow: 0 4px 15px rgba(75, 144, 226, 0.3);
      }
      
      .course-detail-header {
        display: flex;
        gap: 20px;
        margin-bottom: 30px;
        padding: 20px;
        background: linear-gradient(135deg, #4b90e2 0%, #3a7bc8 100%);
        border-radius: 15px;
        align-items: center;
        box-shadow: 0 8px 25px rgba(75, 144, 226, 0.3);
      }
      
      .header-item {
        color: #000;
        font-weight: 600;
        font-size: 16px;
        text-align: center;
        flex: 1;
      }
      
      .header-title {
        flex: 2;
        font-size: 24px;
        font-weight: 700;
        color: #000;
      }
      
      .stats-row {
        display: grid;
        grid-template-columns: 1fr 1fr 1fr;
        gap: 20px;
        margin-bottom: 25px;
      }
      
      .stat-card {
        background: linear-gradient(135deg, #1a1a1a 0%, #2a2a2a 100%);
        border-radius: 12px;
        padding: 20px;
        text-align: center;
        border: 1px solid rgba(255,255,255,0.05);
      }
      
      .stat-label {
        font-size: 12px;
        color: #4b90e2;
        text-transform: uppercase;
        font-weight: 600;
        margin-bottom: 8px;
      }
      
      .stat-value {
        font-size: 20px;
        font-weight: 700;
        color: white;
      }
      
      .requirements-section {
        margin-bottom: 25px;
      }
      
      .requirements-card {
        width: 100%;
        background: linear-gradient(135deg, #1a1a1a 0%, #2a2a2a 100%);
        border-radius: 12px;
        padding: 20px;
        border: 1px solid rgba(255,255,255,0.05);
      }
      
      .requirements-title {
        color: #4b90e2;
        font-size: 14px;
        font-weight: 600;
        margin-bottom: 10px;
        text-transform: uppercase;
      }
      
      .requirements-text {
        color: #b3b3b3;
        font-size: 14px;
        line-height: 1.5;
        margin-bottom: 15px;
      }
      
      .subject-comparison {
        margin-top: 15px;
        padding-top: 15px;
        border-top: 1px solid rgba(255,255,255,0.1);
      }
      
      .comparison-title {
        color: #4b90e2;
        font-size: 12px;
        font-weight: 600;
        margin-bottom: 8px;
        text-transform: uppercase;
      }
      
      .comparison-text {
        color: #b3b3b3;
        font-size: 14px;
        font-weight: bold;
      }
      
      .modal-two-columns {
        display: grid;
        grid-template-columns: 1fr 1fr;
        gap: 20px;
        margin-top: 20px;
      }
      
      .chart-section {
        background: linear-gradient(135deg, #1a1a1a 0%, #2a2a2a 100%);
        border-radius: 15px;
        padding: 25px;
        text-align: center;
        color: #b3b3b3;
        min-height: 250px;
        display: flex;
        align-items: center;
        justify-content: center;
        border: 1px solid rgba(255,255,255,0.05);
        box-shadow: 0 8px 25px rgba(0, 0, 0, 0.3);
      }
      
      .features-section {
        display: flex;
        flex-direction: column;
        gap: 20px;
      }
      
      .features-title {
        color: #4b90e2;
        font-size: 18px;
        font-weight: 700;
        text-transform: uppercase;
        letter-spacing: 1px;
      }
      
      .features-toggle {
        background: linear-gradient(135deg, #4b90e2 0%, #3a7bc8 100%);
        border-radius: 12px;
        padding: 15px 25px;
        color: #000;
        text-align: center;
        cursor: pointer;
        font-weight: 600;
        transition: all 0.3s ease;
        box-shadow: 0 4px 15px rgba(75, 144, 226, 0.3);
      }
      
      .features-toggle:hover {
        transform: translateY(-2px);
        box-shadow: 0 6px 20px rgba(75, 144, 226, 0.4);
      }
      
      .university-btn {
        background: linear-gradient(135deg, #4b90e2 0%, #3a7bc8 100%);
        border: none;
        border-radius: 12px;
        padding: 15px 30px;
        color: #000;
        font-weight: 600;
        cursor: pointer;
        width: 100%;
        transition: all 0.3s ease;
        box-shadow: 0 4px 15px rgba(75, 144, 226, 0.3);
      }
      
      .university-btn:hover {
        transform: translateY(-2px);
        box-shadow: 0 6px 20px rgba(75, 144, 226, 0.4);
      }
      
      .similar-degrees {
        margin-top: 15px;
      }
      
      .similar-degree-card {
        display: grid;
        grid-template-columns: 2fr 1fr 1fr;
        gap: 15px;
        margin-bottom: 15px;
        align-items: center;
        background: linear-gradient(135deg, #1a1a1a 0%, #2a2a2a 100%);
        border-radius: 12px;
        padding: 15px;
        border: 1px solid rgba(255,255,255,0.05);
        transition: all 0.3s ease;
      }
      
      .similar-degree-card:hover {
        transform: translateY(-2px);
        box-shadow: 0 8px 25px rgba(0, 0, 0, 0.3);
      }
      
      .similar-degree-info {
        color: white;
      }
      
      .similar-degree-title {
        font-weight: 600;
        font-size: 16px;
        margin-bottom: 5px;
        color: #4b90e2;
      }
      
      .similar-degree-details {
        font-size: 13px;
        color: #b3b3b3;
      }
      
      .similar-degree-btn {
        background: linear-gradient(135deg, #4b90e2 0%, #3a7bc8 100%);
        border: none;
        border-radius: 8px;
        padding: 8px 15px;
        color: #000;
        font-size: 12px;
        font-weight: 600;
        cursor: pointer;
        transition: all 0.3s ease;
      }
      
      .similar-degree-btn:hover {
        transform: translateY(-1px);
        box-shadow: 0 4px 15px rgba(75, 144, 226, 0.3);
      }
      
      /* RESPONSIVE DESIGN */
      @media (max-width: 768px) {
        .main-content {
          padding: 20px 30px;
        }
        
        .spotify-header {
          padding: 30px 30px;
        }
        
        .main-title {
          font-size: 36px;
        }
        
        .section-header {
          font-size: 24px;
        }
        
        .grades-grid {
          grid-template-columns: 1fr;
        }
        
        .interests-grid {
          grid-template-columns: repeat(auto-fill, minmax(150px, 1fr));
        }
        
        .filter-controls {
          justify-content: flex-start;
        }
        
        .modal-two-columns {
          grid-template-columns: 1fr;
        }
      }
      
      @media (max-width: 480px) {
        .main-content {
          padding: 15px 20px;
        }
        
        .spotify-header {
          padding: 20px 20px;
        }
        
        .main-title {
          font-size: 28px;
        }
        
        .card-container {
          padding: 20px;
        }
      }
      
      /* FIX: Make dropdowns feel seamless and Spotify-like with PROPER positioning */
      .selectize-dropdown {
        z-index: 9999 !important;
        border-radius: 8px !important;
        background: #2a2a2a !important;
        border: 1px solid rgba(255,255,255,0.1) !important;
        box-shadow: 0 6px 18px rgba(0,0,0,0.5) !important;
        color: #ffffff !important;
        font-size: 14px !important;
        padding: 4px 0 !important;
        position: absolute !important;
        top: 100% !important;
        left: 0 !important;
        right: 0 !important;
        transform: none !important;
      }

      .selectize-dropdown-content {
        max-height: 300px;
        overflow-y: auto;
      }

      .selectize-dropdown .option {
        padding: 10px 16px !important;
        transition: background 0.2s ease;
      }

      .selectize-dropdown .option:hover {
        background: rgba(255, 107, 53, 0.2) !important;
        color: #FF6B35 !important;
        cursor: pointer;
      }

      .selectize-dropdown .active {
        background: rgba(255, 107, 53, 0.3) !important;
        color: #FF6B35 !important;
      }

      .selectize-control.single .selectize-input {
        background: rgba(255,255,255,0.08) !important;
        border: 1px solid rgba(255,255,255,0.1) !important;
        border-radius: 8px !important;
        padding: 12px 16px !important;
        color: #ffffff !important;
        font-weight: 500 !important;
        position: relative !important;
      }

      .selectize-control {
        position: relative !important;
      }

      /* FORCE proper positioning */
      .selectize-control .selectize-dropdown {
        position: absolute !important;
        top: calc(100% + 2px) !important;
        left: 0 !important;
        width: 100% !important;
        z-index: 10000 !important;
      }

      /* Prevent interaction with background when dropdown is open */
      body.selectize-dropdown-open {
        pointer-events: auto !important;
      }

      body.selectize-dropdown-open .selectize-dropdown,
      body.selectize-dropdown-open .selectize-input {
        pointer-events: auto !important;
      }
      
      .subject-grade-pair select {
        color: #ffffff !important;
        background-color: rgba(30,30,30,1) !important;
        font-size: 14px !important;
        height: auto !important;
        line-height: 1.5 !important;
      }
      
      .subject-grade-pair select option {
        background: #2a2a2a !important;
        color: #ffffff !important;
      }
      .selectize-input,
      .selectize-input input {
        color: #ffffff !important;
        font-size: 14px !important;
      }
    "))
  ),
  
  # Header
  div(class = "spotify-header",
      div(class = "header-content",
          h1(class = "main-title", "UK Degree Matchmaker"),
          p(class = "main-subtitle", "Match with your perfect university degrees based on grades, interests, and location!")
      )
  ),
  
  # Main Content
  div(class = "main-content",
      
      # Postcode Section
      h2(class = "section-header", "Your Location"),
      p(class = "section-subtitle", "Enter your UK postcode to filter degrees by location (Optional)"),
      
      div(class = "card-container",
          div(class = "postcode-container",
              div(class = "field-label", "UK Postcode"),
              textInput("user_postcode", 
                        label = NULL,
                        value = "",
                        placeholder = "e.g. SW1A 1AA",
                        width = "100%")
          )
      ),
      
      # Grades & Subjects Section
      h2(class = "section-header", "Your Academic Profile"),
      p(class = "section-subtitle", "Tell us your qualifications and grades to help filter by grade-requirement (Optional)"),
      
      div(class = "card-container",
          tabsetPanel(
            id = "qual_layout",
            tabPanel("A Levels", value = "a_levels"),
            tabPanel("A Levels and BTECs", value = "mix"),
            tabPanel("IB", value = "ib") 
          ),
          uiOutput("dynamic_grades_ui")
      ),
      
      # Distance Filter Section
      h2(class = "section-header", "Distance Preferences"),
      p(class = "section-subtitle", "How far are you willing to move for university?"),
      
      div(class = "card-container",
          div(class = "distance-grid",
              div(class = "distance-card", id = "distance_0_10", 
                  onclick = "toggleInterest('distance_0_10', event)", "0‑10 miles"),
              div(class = "distance-card", id = "distance_10_25", 
                  onclick = "toggleInterest('distance_10_25', event)", "10‑25 miles"),
              div(class = "distance-card", id = "distance_25_50", 
                  onclick = "toggleInterest('distance_25_50', event)", "25‑50 miles"),
              div(class = "distance-card", id = "distance_50_100", 
                  onclick = "toggleInterest('distance_50_100', event)", "50‑100 miles"),
              div(class = "distance-card", id = "distance_100_150", 
                  onclick = "toggleInterest('distance_100_150', event)", "100‑150 miles"),
              div(class = "distance-card", id = "distance_150_plus", 
                  onclick = "toggleInterest('distance_150_plus', event)", "150+ miles"),
              div(class = "distance-card", id = "distance_any", 
                  onclick = "toggleInterest('distance_any', event)", "Any Distance")
          )
      ),
      
      # Interests Section - ALL 14 CATEGORIES
      h2(class = "section-header", "Your Interests"),
      p(class = "section-subtitle", "Which subjects interest you the most? We will select only the degrees that relate to each filter you apply"),
      
      div(class = "card-container",
          div(class = "interests-grid",
              div(class = "interest-card", id = "interest_natural_sciences", 
                  onclick = "toggleInterest('natural_sciences', event)", "Natural Sciences"),
              div(class = "interest-card", id = "interest_humanities", 
                  onclick = "toggleInterest('humanities', event)", "Humanities"),
              div(class = "interest-card", id = "interest_architecture", 
                  onclick = "toggleInterest('architecture', event)", "Architecture"),
              div(class = "interest-card", id = "interest_computational", 
                  onclick = "toggleInterest('computational', event)", "Computational & Mathematical Sciences"),
              div(class = "interest-card", id = "interest_social_sciences", 
                  onclick = "toggleInterest('social_sciences', event)", "Social Sciences"),
              div(class = "interest-card", id = "interest_management", 
                  onclick = "toggleInterest('management', event)", "Management"),
              div(class = "interest-card", id = "interest_medicine", 
                  onclick = "toggleInterest('medicine', event)", "Medicine"),
              div(class = "interest-card", id = "interest_sustainability", 
                  onclick = "toggleInterest('sustainability', event)", "Sustainability"),
              div(class = "interest-card", id = "interest_engineering", 
                  onclick = "toggleInterest('engineering', event)", "Engineering"),
              div(class = "interest-card", id = "interest_languages", 
                  onclick = "toggleInterest('languages', event)", "Languages"),
              div(class = "interest-card", id = "interest_arts", 
                  onclick = "toggleInterest('arts', event)", "Arts"),
              div(class = "interest-card", id = "interest_education", 
                  onclick = "toggleInterest('education', event)", "Education"),
              div(class = "interest-card", id = "interest_technology", 
                  onclick = "toggleInterest('technology', event)", "Technology"),
              div(class = "interest-card", id = "interest_law", 
                  onclick = "toggleInterest('law', event)", "Law")
          )
      ),
      
      h2(class = "section-header", "Degree Types"),
      p(class = "section-subtitle", "Filter by specific qualification types"),
      
      div(class = "card-container",
          div(class = "degree-grid",
              # First row - most common degree types
              div(class = "degree-card", id = "interest_bsc", onclick = "toggleInterest('bsc', event)", "BSc"),
              div(class = "degree-card", id = "interest_ba", onclick = "toggleInterest('ba', event)", "BA"),
              div(class = "degree-card", id = "interest_beng", onclick = "toggleInterest('beng', event)", "BEng"),
              div(class = "degree-card", id = "interest_meng", onclick = "toggleInterest('meng', event)", "MEng"),
              div(class = "degree-card", id = "interest_msci", onclick = "toggleInterest('msci', event)", "MSci"),
              div(class = "degree-card", id = "interest_llb", onclick = "toggleInterest('llb', event)", "LLB"),
              
              # Second row - specialized degrees
              div(class = "degree-card small-text", id = "interest_mbbs", onclick = "toggleInterest('mbbs', event)", "MBBS"),
              div(class = "degree-card small-text", id = "interest_mbbsbsc", onclick = "toggleInterest('mbbsbsc', event)", "MBBS BSc"),
              div(class = "degree-card", id = "interest_mpharm", onclick = "toggleInterest('mpharm', event)", "MPharm"),
              div(class = "degree-card", id = "interest_basc", onclick = "toggleInterest('basc', event)", "BASc"),
              div(class = "degree-card", id = "interest_bfa", onclick = "toggleInterest('bfa', event)", "BFA"),
              div(class = "degree-card", id = "interest_mbio", onclick = "toggleInterest('mbio', event)", "MBio"),
              
              # Third row - specialized Master's degrees
              div(class = "degree-card", id = "interest_mchem", onclick = "toggleInterest('mchem', event)", "MChem"),
              div(class = "degree-card", id = "interest_mphys", onclick = "toggleInterest('mphys', event)", "MPhys"),
              div(class = "degree-card small-text", id = "interest_mmathphys", onclick = "toggleInterest('mmathphys', event)", "MMathPhys"),
              div(class = "degree-card", id = "interest_mmath", onclick = "toggleInterest('mmath', event)", "MMath"),
              div(class = "degree-card small-text", id = "interest_mmathstat", onclick = "toggleInterest('mmathstat', event)", "MMathStat"),
              div(class = "degree-card", id = "interest_mmorse", onclick = "toggleInterest('mmorse', event)", "MMORSE"),
              
              # Fourth row - combined degrees
              div(class = "degree-card small-text", id = "interest_babsc", onclick = "toggleInterest('babsc', event)", "BA/BSc")
          )
      ),
      
      # University Filter Section - NEW
      h2(class = "section-header", "Filter by University"),
      p(class = "section-subtitle", "Select the specific universities that you're interested in"),
      
      div(class = "card-container",
          div(class = "postcode-container", style = "max-width: 100%; margin: 0;",
              div(class = "field-label", "Select Universities"),
              selectizeInput(
                "selected_universities", 
                label = NULL,
                choices = universities,
                multiple = TRUE,
                options = list(
                  placeholder = "Select universities...",
                  maxItems = 10,
                  plugins = list('remove_button'),
                  create = FALSE,
                  hideSelected = TRUE
                )
              )
          )
      ),
      
      # Search Button
      actionButton("submit_filters", "Find My Perfect Courses", class = "spotify-btn"),
      
      # Filter Section
      div(class = "filter-section",
          div(style = "display: flex; justify-content: space-between; align-items: center; flex-wrap: wrap; gap: 20px;",
              # Filter Tabs
              div(class = "filter-tabs",
                  tags$button("All Suitable Courses", class = "filter-tab active", id = "tab_all", 
                              onclick = "switchTab('all')"),
                  tags$button("Exact Grade Matches", class = "filter-tab", id = "tab_exact", 
                              onclick = "switchTab('exact')"),
                  tags$button("Overqualified Matches", class = "filter-tab", id = "tab_over", 
                              onclick = "switchTab('over')"),
                  tags$button("Underqualified Matches", class = "filter-tab", id = "tab_under", 
                              onclick = "switchTab('under')")
              ),
              # Filter Controls
              div(class = "filter-controls",
                  div(style = "display: flex; align-items: center; gap: 8px;",
                      tags$label("Sort by:", `for` = "sort_by"),
                      selectInput("sort_by", NULL,
                                  choices = list("Match Quality" = "match", "Grade Requirement" = "grade", "Distance" = "distance", "Alphabetical" = "alpha"),
                                  selected = "match",
                                  width = "140px")
                  ),
                  div(style = "display: flex; align-items: center; gap: 8px;",
                      tags$label("Show:", `for` = "num_courses"),
                      selectInput("num_courses", NULL,
                                  choices = list("4" = 4, "16" = 16, "64" = 64, "All" = "all"),
                                  selected = 4,
                                  width = "100px")
                  )
              )
          )
      ),
      
      # Course Results
      h2(class = "section-header", "Your Course Matches"),
      p(class = "section-subtitle", "Courses tailored to your academic profile, interests, and location"),
      
      div(class = "courses-grid",
          uiOutput("course_cards")
      )
  ),
  
  # Full Modal with all functionality (same as before but updated field names)
  div(id = "courseModal", class = "custom-modal",
      div(class = "modal-content-custom",
          span(class = "modal-close", "×"),
          
          # Back button
          tags$button("← Go Back", class = "go-back-btn", onclick = "closeModal()"),
          
          # Single row header with course info + grade match + subject requirements
          div(class = "course-detail-header",
              div(class = "header-item", id = "modal_degree_type", "BSc"),
              div(class = "header-title", id = "modal_subject_title", "Subject Title"),
              div(class = "header-item", id = "modal_university_name", "University"),
              div(class = "header-item", id = "modal_grade_req", "Grade Req: A"),
              div(style = "flex: 1; display: flex; justify-content: center; align-items: center; gap: 8px;",
                  div(style = "background: #4CAF50; color: white; padding: 8px 16px; border-radius: 8px; font-size: 12px; font-weight: 600;", 
                      id = "modal_match_badge", "Exact Match"),
                  div(style = "background: #2196F3; color: white; padding: 8px 16px; border-radius: 8px; font-size: 12px; font-weight: 600; display: none;", 
                      id = "modal_subject_badge", "Subject Requirements Met"),
                  div(style = "background: rgba(46, 134, 171, 0.8); color: white; padding: 8px 16px; border-radius: 8px; font-size: 12px; font-weight: 600;", 
                      id = "modal_distance_badge", "Distance: 25 miles")
              )
          ),
          
          # Stats row (updated with offer rate)
          div(class = "stats-row",
              div(class = "stat-card",
                  div(class = "stat-label", "Median Salary"),
                  div(class = "stat-value", id = "modal_salary", "£30,000")
              ),
              div(class = "stat-card",
                  div(class = "stat-label", "Offer Rate"),
                  div(class = "stat-value", id = "modal_offer_rate", "Data Not Available")
              ),
              div(class = "stat-card",
                  div(class = "stat-label", "Course Duration"),
                  div(class = "stat-value", id = "modal_duration", "3 Years")
              )
          ),
          
          # Requirements textbox with matched subjects indicator
          div(class = "requirements-section",
              div(class = "requirements-card",
                  div(class = "requirements-title", "Requirements & Options"),
                  div(class = "requirements-text", id = "modal_requirements", 
                      "A-Level Subjects: Mathematics, Physics or Chemistry required. Year abroad available. Sandwich placement year optional."),
                  # Subject comparison indicator
                  div(class = "subject-comparison", id = "matched_subjects_indicator",
                      div(class = "comparison-title", "How Your Subjects Compare"),
                      div(class = "comparison-text", id = "matched_subjects_list", "Select subjects to see comparison")
                  )
              )
          ),
          
          # Two column layout
          div(class = "modal-two-columns",
              div(class = "features-section",
                  tags$button("View on University Website", class = "university-btn", 
                              id = "university_website_btn", onclick = ""),
                  div(class = "similar-degrees",
                      div(class = "features-title", "You May Be Interested In:"),
                      div(id = "similar_degrees_container",
                          div(class = "similar-degree-card",
                              div(class = "similar-degree-info",
                                  div(class = "similar-degree-title", "Loading..."),
                                  div(class = "similar-degree-details", "Finding similar courses...")
                              ),
                              tags$button("Course URL", class = "similar-degree-btn"),
                              tags$button("Learn More", class = "similar-degree-btn")
                          )
                      )
                  )
              )
          )
      )
  ),
  
  # JavaScript for modal and toggle functionality
  tags$script(HTML("
    // Store scroll position before updates
    var lastScrollPosition = 0;
    
    // Function to save scroll position
    function saveScrollPosition() {
      lastScrollPosition = window.pageYOffset || document.documentElement.scrollTop;
    }
    
    // Function to restore scroll position
    function restoreScrollPosition() {
      window.scrollTo(0, lastScrollPosition);
    }
    
    // Save scroll position before any Shiny updates
    $(document).on('shiny:inputchanged', function(event) {
      if (event.name === 'sort_by' || event.name === 'num_courses') {
        saveScrollPosition();
        // Restore position after a short delay to allow for DOM updates
        setTimeout(restoreScrollPosition, 100);
      }
    });
    
    // Modal functions
    function openModal(courseIndex) {
      document.getElementById('courseModal').style.display = 'block';
      Shiny.setInputValue('selected_course_index', courseIndex);
    }
    
    function closeModal() {
      document.getElementById('courseModal').style.display = 'none';
    }
    
    // Close modal when clicking outside
    window.onclick = function(event) {
      var modal = document.getElementById('courseModal');
      if (event.target == modal) {
        closeModal();
      }
    }
    
    // Close modal when clicking X
    document.querySelector('.modal-close').onclick = function() {
      closeModal();
    }
    
    // Tab switching functionality
    function switchTab(tabName) {
      saveScrollPosition();
      // Update active tab styling
      document.querySelectorAll('.filter-tab').forEach(btn => btn.classList.remove('active'));
      document.getElementById('tab_' + tabName).classList.add('active');
      
      // Send tab change to Shiny
      Shiny.setInputValue('current_tab', tabName);
      
      // Restore scroll position after tab change
      setTimeout(restoreScrollPosition, 100);
    }
    
    // Toggle interest subjects - FIXED to prevent click-through
    function toggleInterest(subject, event) {
      if (event) {
        event.stopPropagation();
        event.preventDefault();
      }
      var element = document.getElementById('interest_' + subject);
      if (!element) {
        element = document.getElementById(subject);
      }
      if (element) {
        element.classList.toggle('selected');
        Shiny.setInputValue('interest_' + subject, element.classList.contains('selected'));
      }
    }
    
    // Update modal content - FIXED VERSION with matched subjects and conditional distance
    Shiny.addCustomMessageHandler('updateModal', function(data) {
      document.getElementById('modal_degree_type').innerText = data.degree_type;
      document.getElementById('modal_subject_title').innerText = data.title;
      document.getElementById('modal_university_name').innerText = data.university;
      var gradeReqElem = document.getElementById('modal_grade_req');
      gradeReqElem.innerText = data.grade_req;
      gradeReqElem.classList.remove('badge-alevel', 'badge-ib');
      if (data.grade_type === 'ib') {
        gradeReqElem.classList.add('badge-ib');
      } else {
        gradeReqElem.classList.add('badge-alevel');
      }
      document.getElementById('modal_salary').innerText = data.salary;
      document.getElementById('modal_offer_rate').innerText = data.offer_rate;

      // Update distance badge - only show if distance is available
      var distanceBadge = document.getElementById('modal_distance_badge');
      if(data.distance_text && data.distance_text !== null) {
        distanceBadge.innerText = data.distance_text;
        distanceBadge.style.display = 'block';
      } else {
        distanceBadge.style.display = 'none';
      }

      // Update requirements text with real data
      document.getElementById('modal_requirements').innerText = data.requirements;

      // Update match badge with proper color
      var matchBadge = document.getElementById('modal_match_badge');
      var matchColor = '#666666'; // default
      switch(data.match_type) {
        case 'Exact Match': matchColor = '#4CAF50'; break;
        case 'Good Match': matchColor = '#2196F3'; break;
        case 'Overmatch': matchColor = '#FF9800'; break;
        case 'No Match': matchColor = '#f44336'; break;
      }
      matchBadge.style.backgroundColor = matchColor;
      matchBadge.innerText = data.match_type;

      // Update subject comparison display
      var matchedSubjectsContainer = document.getElementById('matched_subjects_indicator');
      var matchedSubjectsList = document.getElementById('matched_subjects_list');
      
      // Always show the section
      matchedSubjectsContainer.style.display = 'block';
      
      if (!data.has_subject_selection) {
        // User hasn't selected any subjects yet
        matchedSubjectsList.innerText = 'Select subjects to see comparison';
        matchedSubjectsList.style.color = '#b3b3b3'; // Neutral gray
      } else if (data.matched_subjects && data.matched_subjects.length > 0) {
        // User has selected subjects and found matches
        matchedSubjectsList.innerText = data.matched_subjects.join(', ');
        matchedSubjectsList.style.color = '#4b90e2'; // Blue for matches
      } else {
        // User has selected subjects but no matches found
        matchedSubjectsList.innerText = 'No matching A-Level subjects found';
        matchedSubjectsList.style.color = '#f44336'; // Red for no matches
      }

      // Update university website button
      document.getElementById('university_website_btn').onclick = function() {
        window.open(data.url, '_blank');
      };

      // Update similar degrees
      var similarContainer = document.getElementById('similar_degrees_container');
      if(data.similar_courses && data.similar_courses.length > 0) {
        var similarHTML = '';
        data.similar_courses.forEach(function(course, index) {
          similarHTML += '<div class=\"similar-degree-card\">' +
            '<div class=\"similar-degree-info\">' +
              '<div class=\"similar-degree-title\">' + course.title + '</div>' +
              '<div class=\"similar-degree-details\">' + course.university + ' ' + course.degree_type + ' Grade Req: ' + course.grade_req + '</div>' +
            '</div>' +
            '<button class=\"similar-degree-btn\" onclick=\"window.open(\\'' + course.url + '\\', \\'_blank\\')\">Course URL</button>' +
            '<button class=\"similar-degree-btn\" onclick=\"openModal(' + course.index + ')\">Learn More</button>' +
          '</div>';
        });
        similarContainer.innerHTML = similarHTML;
      } else {
        similarContainer.innerHTML = '<div style=\"text-align: center; color: #b3b3b3; padding: 20px;\">No similar courses found</div>';
      }
    });
    
    // Handle scroll preservation for filter/sort changes
    Shiny.addCustomMessageHandler('preserveScroll', function(data) {
      setTimeout(function() {
        restoreScrollPosition();
      }, 150);
    });
    
    // Update just the subject comparison section when subjects change
    Shiny.addCustomMessageHandler('updateSubjectComparison', function(data) {
      console.log('Updating subject comparison with:', data);
      
      var matchedSubjectsContainer = document.getElementById('matched_subjects_indicator');
      var matchedSubjectsList = document.getElementById('matched_subjects_list');
      
      if (!matchedSubjectsContainer || !matchedSubjectsList) {
        console.log('Modal elements not found');
        return;
      }
      
      // Always show the section
      matchedSubjectsContainer.style.display = 'block';
      
      if (!data.has_subject_selection) {
        // User hasn't selected any subjects yet
        matchedSubjectsList.innerText = 'Select subjects to see comparison';
        matchedSubjectsList.style.color = '#b3b3b3'; // Neutral gray
        console.log('No subjects selected');
      } else if (data.matched_subjects && data.matched_subjects.length > 0) {
        // User has selected subjects and found matches
        matchedSubjectsList.innerText = data.matched_subjects.join(', ');
        matchedSubjectsList.style.color = '#4b90e2'; // Blue for matches
        console.log('Matches found:', data.matched_subjects);
      } else {
        // User has selected subjects but no matches found
        matchedSubjectsList.innerText = 'No matching A-Level subjects found';
        matchedSubjectsList.style.color = '#f44336'; // Red for no matches
        console.log('No matches found');
      }
    });
  "))
)

# FULL SERVER FUNCTIONALITY WITH DISTANCE CALCULATION
server <- function(input, output, session) {
  
  similar_courses_cache <- reactiveValues()
  
  # ...inside your server function, after the opening line and before current_selected_subjects...
  
  # ...inside your server function...
  
  output$dynamic_grades_ui <- renderUI({
    layout <- input$qual_layout
    if (is.null(layout) || layout == "a_levels") {
      # 4 A Levels, 4 columns per row
      div(class = "grades-grid",
          lapply(1:4, function(i) {
            label_suffix <- if (i == 4) " (Optional)" else ""
            list(
              div(class = "subject-grade-pair",
                  div(class = "field-label", paste("A Level Subject", i, label_suffix)),
                  selectizeInput(paste0("subject", i), label = NULL, choices = c("Select Subject" = "", all_subjects), multiple = FALSE)
              ),
              div(class = "subject-grade-pair",
                  div(class = "field-label", paste("Grade", i, label_suffix)),
                  selectInput(paste0("grade", i), label = NULL, choices = c("Select Grade" = "", "A*", "A", "B", "C", "D", "E"), selectize = FALSE)
              )
            )
          })
      )
    } else if (layout == "mix") {
      # 4 rows, each with 3 columns (Type, Subject, Grade)
      div(class = "grades-grid-btec",
          lapply(1:4, function(i) {
            label_suffix <- if (i == 4) " (Optional)" else ""
            list(
              div(class = "subject-grade-pair",
                  div(class = "field-label", paste("Type", i, label_suffix)),
                  selectInput(paste0("type", i), label = NULL, choices = c("A Level", "BTEC"), selected = "A Level", selectize = FALSE)
              ),
              div(class = "subject-grade-pair",
                  div(class = "field-label", paste("Subject", i, label_suffix)),
                  uiOutput(paste0("mix_subject_ui_", i))
              ),
              div(class = "subject-grade-pair",
                  div(class = "field-label", paste("Grade", i, label_suffix)),
                  uiOutput(paste0("mix_grade_ui_", i))
              )
            )
          })
      )
    } else if (layout == "ib") {
      ib_group1 <- c("Literature HL", "Literature SL", "Language and Literature HL", "Language and Literature SL", "Literature and Performance SL")
      ib_group2 <- c("Language B HL", "Language B SL", "Language ab initio SL", "Classical Languages HL", "Classical Languages SL")
      ib_group3 <- c("Business Management HL", "Business Management SL", "Economics HL", "Economics SL", "Geography HL", "Geography SL", "Global Politics HL", "Global Politics SL", "History HL", "History SL", "ITGS HL", "ITGS SL", "Philosophy HL", "Philosophy SL", "Psychology HL", "Psychology SL", "Social and Cultural Anthropology HL", "Social and Cultural Anthropology SL", "World Religions SL")
      ib_group4 <- c("Biology HL", "Biology SL", "Chemistry HL", "Chemistry SL", "Physics HL", "Physics SL", "Computer Science HL", "Computer Science SL", "Design Technology HL", "Design Technology SL", "Environmental Systems and Societies HL", "Environmental Systems and Societies SL", "Sports, Exercise and Health Science HL", "Sports, Exercise and Health Science SL")
      ib_group5 <- c("Mathematics: Analysis and Approaches HL", "Mathematics: Analysis and Approaches SL", "Mathematics: Applications and Interpretation HL", "Mathematics: Applications and Interpretation SL")
      ib_group6 <- c("Dance HL", "Dance SL", "Film HL", "Film SL", "Music HL", "Music SL", "Theatre HL", "Theatre SL", "Visual Arts HL", "Visual Arts SL", "Any other subject from Groups 1-4")
      ib_grade_choices <- as.character(7:1)
      div(class = "grades-grid",
          # Group 1
          div(class = "subject-grade-pair",
              div(class = "field-label", "Group 1: Studies in Language and Literature"),
              selectizeInput("ib_g1_subject", label = NULL, choices = c("Select Subject" = "", ib_group1), multiple = FALSE),
              div(class = "field-label", "Grade"),
              selectInput("ib_g1_grade", label = NULL, choices = c("", ib_grade_choices), selectize = FALSE)
          ),
          # Group 2
          div(class = "subject-grade-pair",
              div(class = "field-label", "Group 2: Language Acquisition"),
              selectizeInput("ib_g2_subject", label = NULL, choices = c("Select Subject" = "", ib_group2), multiple = FALSE),
              div(class = "field-label", "Grade"),
              selectInput("ib_g2_grade", label = NULL, choices = c("", ib_grade_choices), selectize = FALSE)
          ),
          # Group 3
          div(class = "subject-grade-pair",
              div(class = "field-label", "Group 3: Individuals and Societies"),
              selectizeInput("ib_g3_subject", label = NULL, choices = c("Select Subject" = "", ib_group3), multiple = FALSE),
              div(class = "field-label", "Grade"),
              selectInput("ib_g3_grade", label = NULL, choices = c("", ib_grade_choices), selectize = FALSE)
          ),
          # Group 4
          div(class = "subject-grade-pair",
              div(class = "field-label", "Group 4: Sciences"),
              selectizeInput("ib_g4_subject", label = NULL, choices = c("Select Subject" = "", ib_group4), multiple = FALSE),
              div(class = "field-label", "Grade"),
              selectInput("ib_g4_grade", label = NULL, choices = c("", ib_grade_choices), selectize = FALSE)
          ),
          # Group 5
          div(class = "subject-grade-pair",
              div(class = "field-label", "Group 5: Mathematics"),
              selectizeInput("ib_g5_subject", label = NULL, choices = c("Select Subject" = "", ib_group5), multiple = FALSE),
              div(class = "field-label", "Grade"),
              selectInput("ib_g5_grade", label = NULL, choices = c("", ib_grade_choices), selectize = FALSE)
          ),
          # Group 6
          div(class = "subject-grade-pair",
              div(class = "field-label", "Group 6: The Arts or Substitute"),
              selectizeInput("ib_g6_subject", label = NULL, choices = c("Select Subject" = "", ib_group6), multiple = FALSE),
              div(class = "field-label", "Grade"),
              selectInput("ib_g6_grade", label = NULL, choices = c("", ib_grade_choices), selectize = FALSE)
          ),
          # Core bonus points (full width)
          div(style = "grid-column: 1 / -1; margin-top: 16px;",
              div(class = "field-label", "Core Bonus Points (TOK/EE)"),
              selectInput("ib_core_bonus", label = NULL, choices = 0:3, selected = 0, selectize = FALSE)
          )
      )
    }
  })
  
  # Render subject and grade inputs for each row in "mix" tab
  for (i in 1:4) {
    local({
      idx <- i
      output[[paste0("mix_subject_ui_", idx)]] <- renderUI({
        type <- input[[paste0("type", idx)]]
        if (is.null(type) || type == "A Level") {
          selectizeInput(paste0("subject", idx), label = NULL, choices = c("Select Subject" = "", all_subjects), multiple = FALSE)
        } else {
          selectizeInput(paste0("subject", idx), label = NULL, choices = c("Select Subject" = "", btec_subjects), multiple = FALSE)
        }
      })
      output[[paste0("mix_grade_ui_", idx)]] <- renderUI({
        type <- input[[paste0("type", idx)]]
        if (is.null(type) || type == "A Level") {
          selectInput(paste0("grade", idx), label = NULL, choices = c("Select Grade" = "", "A*", "A", "B", "C", "D", "E"), selectize = FALSE)
        } else {
          selectInput(paste0("grade", idx), label = NULL, choices = c("Select Grade" = "", btec_grades), selectize = FALSE)
        }
      })
    })
  }
  
  # Reactive function to get current selected subjects (updates dynamically)
  current_selected_subjects <- reactive({
    get_selected_subjects(input)
  })
  
  # FIXED: Proper debounced postcode reactive
  user_postcode_debounced <- reactive({
    input$user_postcode
  }) %>% debounce(1500)  # Increased to 1.5 seconds
  
  # OPTIMIZED: Distance calculation only for unique addresses
  distances_calculated <- reactive({
    user_postcode <- user_postcode_debounced()
    
    if(is.null(user_postcode) || user_postcode == "" || trimws(user_postcode) == "") {
      # No postcode provided, return data without distance calculations
      data_with_distance <- degree_data
      data_with_distance$distance_miles <- NA
      data_with_distance$distance_range <- NA
      data_with_distance$has_distance <- FALSE
      return(data_with_distance)
    }
    
    # Clean user postcode
    user_postcode <- toupper(trimws(user_postcode))
    
    tryCatch({
      # OPTIMIZATION: Get unique addresses only
      unique_addresses <- unique(degree_data$provaddress[!is.na(degree_data$provaddress) & degree_data$provaddress != ""])
      
      # Show optimization info and progress
      cat("OPTIMIZATION: Processing", length(unique_addresses), "unique addresses instead of", nrow(degree_data), "courses\n")
      showNotification(paste("Smart optimization: Calculating distances for only", length(unique_addresses), 
                             "unique locations instead of", nrow(degree_data), "courses!"), 
                       type = "message", duration = 3)
      
      # Calculate distances only for unique addresses
      distance_lookup <- data.frame(
        provaddress = unique_addresses,
        stringsAsFactors = FALSE
      )
      
      # Apply degree_dist function to unique addresses only
      distance_results <- lapply(seq_along(unique_addresses), function(i) {
        address <- unique_addresses[i]
        
        # Show progress every 10 addresses
        if(i %% 10 == 0 || i == length(unique_addresses)) {
          showNotification(paste("Processing location", i, "of", length(unique_addresses)), 
                           type = "message", duration = 1)
        }
        
        result <- degree_dist(user_postcode, address)
        if(nrow(result) > 0) {
          return(list(miles = result$miles[1], distance_range = as.character(result$distance_range[1])))
        } else {
          return(list(miles = NA, distance_range = "Error"))
        }
      })
      
      # Create lookup table
      distance_lookup$distance_miles <- sapply(distance_results, function(x) x$miles)
      distance_lookup$distance_range <- sapply(distance_results, function(x) x$distance_range)
      
      # Map results back to all courses using merge (preserve all columns)
      data_with_distance <- merge(degree_data, distance_lookup, by = "provaddress", all.x = TRUE, sort = FALSE)
      
      # Handle courses without address matches
      data_with_distance$distance_miles[is.na(data_with_distance$distance_miles)] <- NA
      data_with_distance$distance_range[is.na(data_with_distance$distance_range)] <- "Unknown"
      data_with_distance$has_distance <- TRUE
      
      # Restore original order (by grade score, highest first)
      data_with_distance <- data_with_distance[order(-data_with_distance$grade_score, na.last = TRUE), ]
      
      showNotification("Distances calculated!", type = "message", duration = 1)
      return(data_with_distance)
      
    }, error = function(e) {
      cat("Error in distance calculation:", e$message, "\n")
      data_with_distance <- degree_data
      data_with_distance$distance_miles <- NA
      data_with_distance$distance_range <- "Error"
      data_with_distance$has_distance <- FALSE
      showNotification("Error calculating distances", type = "error", duration = 3)
      return(data_with_distance)
    })
  })
  
  # Main filtering logic (triggered by submit button) - OPTIMIZED
  filtered_courses <- eventReactive(input$submit_filters, {
    data <- distances_calculated()  # Use reactive function correctly
    
    # Get selected subjects
    student_subjects <- get_selected_subjects(input)
    
    layout <- input$qual_layout
    student_grades <- c()
    student_score <- NA
    ib_score <- NA
    
    if (is.null(layout) || layout == "a_levels") {
      student_grades <- c(input$grade1, input$grade2, input$grade3, input$grade4)
      student_grades <- student_grades[!is.null(student_grades) & student_grades != ""]
      if(length(student_grades) >= 3) {
        student_score <- calculate_student_score(student_grades)
      }
    } else if (layout == "mix") {
      for (i in 1:4) {
        g <- input[[paste0("grade", i)]]
        if (!is.null(g) && g != "") student_grades <- c(student_grades, g)
      }
      student_grades <- student_grades[!is.null(student_grades) & student_grades != ""]
      if(length(student_grades) >= 3) {
        student_score <- calculate_student_score(student_grades)
      }
    } else if (layout == "ib") {
      ib <- get_ib_grades(input)
      if(length(ib$grades) == 6 && !is.na(ib$bonus)) {
        ib_score <- calculate_ib_score(ib$grades, ib$bonus)
      }
    }
    if(layout == "ib" && !is.na(ib_score)) {
      data$ib_score <- ib_score
      data$ib_match_type <- sapply(data$ib_grade_req, function(req) {
        req_num <- suppressWarnings(as.numeric(req))
        if(is.na(req_num)) return("No Data")
        diff <- ib_score - req_num
        if(diff == 0) return("Exact Match")
        else if(diff > 0) return("Overmatch")
        else return("Under Match")
      })
      data$ib_score_diff <- sapply(data$ib_grade_req, function(req) {
        req_num <- suppressWarnings(as.numeric(req))
        if(is.na(req_num)) return(NA)
        ib_score - req_num
      })
    } else if(!is.na(student_score)) {
      data$student_score <- student_score
      data$match_type <- sapply(data$grade_score, function(course_score) {
        get_match_type(student_score, course_score)
      })
      data$score_diff <- sapply(data$grade_score, function(course_score) {
        if(is.na(course_score)) return(NA)
        student_score - course_score
      })
    }
    
    if(layout == "ib" && !is.na(ib_score)) {
      match_order <- c("Exact Match", "Overmatch", "Under Match", "No Data")
      data$match_priority <- match(data$ib_match_type, match_order)
      data <- data[order(data$match_priority, -as.numeric(data$ib_grade_req), na.last = TRUE), ]
    } else if(!is.na(student_score)) {
      match_order <- c("Exact Match", "Good Match", "Overmatch", "No Match", "No Data")
      data$match_priority <- match(data$match_type, match_order)
      data <- data[order(data$match_priority, -data$grade_score, na.last = TRUE), ]
    } else {
      data$match_type <- "No Data"
      data$match_priority <- 5
      student_score <- NA
    }
    
    # Add subject requirements matching - UPDATED COLUMN NAME
    if(length(student_subjects) > 0) {
      data$subject_requirements_met <- sapply(1:nrow(data), function(i) {
        course_requirements <- if(!is.null(data$a_level_subject_reqs[i]) && !is.na(data$a_level_subject_reqs[i])) {
          data$a_level_subject_reqs[i]
        } else {
          ""
        }
        match_subjects_with_requirements(student_subjects, course_requirements)
      })
      
      data$selected_subjects <- list(student_subjects)
    } else {
      data$subject_requirements_met <- FALSE
      data$selected_subjects <- list(character(0))
    }
    
    # Store student score for later use
    data$student_score <- student_score
    
    # Apply distance filters
    selected_distances <- c()
    if(!is.null(input$interest_distance_0_10) && input$interest_distance_0_10) selected_distances <- c(selected_distances, "0‑10 miles")
    if(!is.null(input$interest_distance_10_25) && input$interest_distance_10_25) selected_distances <- c(selected_distances, "10‑25 miles")
    if(!is.null(input$interest_distance_25_50) && input$interest_distance_25_50) selected_distances <- c(selected_distances, "25‑50 miles")
    if(!is.null(input$interest_distance_50_100) && input$interest_distance_50_100) selected_distances <- c(selected_distances, "50‑100 miles")
    if(!is.null(input$interest_distance_100_150) && input$interest_distance_100_150) selected_distances <- c(selected_distances, "100‑150 miles")
    if(!is.null(input$interest_distance_150_plus) && input$interest_distance_150_plus) selected_distances <- c(selected_distances, "150+ miles")
    if(!is.null(input$interest_distance_any) && input$interest_distance_any) selected_distances <- c(selected_distances, "Any Distance")
    
    # Apply distance filtering (only if user has entered a postcode and selected distance filters)
    if(length(selected_distances) > 0 && !"Any Distance" %in% selected_distances && 
       !is.null(input$user_postcode) && input$user_postcode != "" && any(!is.na(data$distance_range))) {
      data <- data[data$distance_range %in% selected_distances | is.na(data$distance_range), ]
    }
    
    # Filter by degree types
    selected_degrees <- c()
    if(!is.null(input$interest_bsc) && input$interest_bsc) selected_degrees <- c(selected_degrees, "BSc")
    if(!is.null(input$interest_ba) && input$interest_ba) selected_degrees <- c(selected_degrees, "BA")
    if(!is.null(input$interest_beng) && input$interest_beng) selected_degrees <- c(selected_degrees, "BEng")
    if(!is.null(input$interest_meng) && input$interest_meng) selected_degrees <- c(selected_degrees, "MEng")
    if(!is.null(input$interest_msci) && input$interest_msci) selected_degrees <- c(selected_degrees, "MSci")
    if(!is.null(input$interest_llb) && input$interest_llb) selected_degrees <- c(selected_degrees, "LLB")
    if(!is.null(input$interest_mbbs) && input$interest_mbbs) selected_degrees <- c(selected_degrees, "MBBS")
    if(!is.null(input$interest_mbbsbsc) && input$interest_mbbsbsc) selected_degrees <- c(selected_degrees, "MBBS BSc")
    if(!is.null(input$interest_mpharm) && input$interest_mpharm) selected_degrees <- c(selected_degrees, "MPharm")
    if(!is.null(input$interest_basc) && input$interest_basc) selected_degrees <- c(selected_degrees, "BASc")
    if(!is.null(input$interest_bfa) && input$interest_bfa) selected_degrees <- c(selected_degrees, "BFA")
    if(!is.null(input$interest_mbio) && input$interest_mbio) selected_degrees <- c(selected_degrees, "MBio")
    if(!is.null(input$interest_mchem) && input$interest_mchem) selected_degrees <- c(selected_degrees, "MChem")
    if(!is.null(input$interest_mphys) && input$interest_mphys) selected_degrees <- c(selected_degrees, "MPhys")
    if(!is.null(input$interest_mmathphys) && input$interest_mmathphys) selected_degrees <- c(selected_degrees, "MMathPhys")
    if(!is.null(input$interest_mmath) && input$interest_mmath) selected_degrees <- c(selected_degrees, "MMath")
    if(!is.null(input$interest_mmathstat) && input$interest_mmathstat) selected_degrees <- c(selected_degrees, "MMathStat")
    if(!is.null(input$interest_mmorse) && input$interest_mmorse) selected_degrees <- c(selected_degrees, "MMORSE")
    if(!is.null(input$interest_babsc) && input$interest_babsc) selected_degrees <- c(selected_degrees, "BA/BSc")
    
    # Filter by subject interests
    selected_subjects <- c()
    if(!is.null(input$interest_natural_sciences) && input$interest_natural_sciences) {
      selected_subjects <- c(selected_subjects, "Natural Sciences")
    }
    if(!is.null(input$interest_humanities) && input$interest_humanities) {
      selected_subjects <- c(selected_subjects, "Humanities")
    }
    if(!is.null(input$interest_architecture) && input$interest_architecture) {
      selected_subjects <- c(selected_subjects, "Architecture")
    }
    if(!is.null(input$interest_computational) && input$interest_computational) {
      selected_subjects <- c(selected_subjects, "Computational & Mathematical Sciences")
    }
    if(!is.null(input$interest_social_sciences) && input$interest_social_sciences) {
      selected_subjects <- c(selected_subjects, "Social Sciences")
    }
    if(!is.null(input$interest_management) && input$interest_management) {
      selected_subjects <- c(selected_subjects, "Management")
    }
    if(!is.null(input$interest_medicine) && input$interest_medicine) {
      selected_subjects <- c(selected_subjects, "Medicine")
    }
    if(!is.null(input$interest_sustainability) && input$interest_sustainability) {
      selected_subjects <- c(selected_subjects, "Sustainability")
    }
    if(!is.null(input$interest_engineering) && input$interest_engineering) {
      selected_subjects <- c(selected_subjects, "Engineering")
    }
    if(!is.null(input$interest_languages) && input$interest_languages) {
      selected_subjects <- c(selected_subjects, "Languages")
    }
    if(!is.null(input$interest_arts) && input$interest_arts) {
      selected_subjects <- c(selected_subjects, "Arts")
    }
    if(!is.null(input$interest_education) && input$interest_education) {
      selected_subjects <- c(selected_subjects, "Education")
    }
    if(!is.null(input$interest_technology) && input$interest_technology) {
      selected_subjects <- c(selected_subjects, "Technology")
    }
    if(!is.null(input$interest_law) && input$interest_law) {
      selected_subjects <- c(selected_subjects, "Law")
    }
    
    # Apply subject filter using OR logic
    if(length(selected_subjects) > 0) {
      # Collect all courses that match ANY of the selected subjects
      all_matching_courses <- data.frame()
      
      for(subject in selected_subjects) {
        subject_courses <- get_subject_category_courses(subject, degree_data)  # Use original dataset
        all_matching_courses <- rbind(all_matching_courses, subject_courses)
      }
      
      # Remove duplicates (courses that appear in multiple categories)
      subject_filtered_data <- all_matching_courses[!duplicated(all_matching_courses$title), ]
      
      # Keep only courses that exist in both the subject filter AND our working dataset
      data <- data[data$title %in% subject_filtered_data$title, ]
    }
    
    # Apply degree filter (only if degrees are selected)
    if(length(selected_degrees) > 0) {
      data <- data[data$degree_type %in% selected_degrees, ]
    }
    
    # Apply university filter (only if universities are selected)
    if(!is.null(input$selected_universities) && length(input$selected_universities) > 0) {
      data <- data[data$university_name %in% input$selected_universities, ]
    }
    
    return(data)
  }, ignoreNULL = FALSE)
  
  # Store the currently displayed courses for modal access
  displayed_courses <- reactive({
    if(input$submit_filters == 0) {
      filtered_data <- distances_calculated()
      filtered_data$match_type <- "No Data"
      filtered_data$subject_requirements_met <- FALSE
      filtered_data$selected_subjects <- list(character(0))
      if(is.null(filtered_data$has_distance)) {
        filtered_data$has_distance <- FALSE
      }
    } else {
      filtered_data <- filtered_courses()
    }
    
    current_tab <- if(is.null(input$current_tab)) "all" else input$current_tab
    layout <- input$qual_layout
    if (is.null(layout)) layout <- "a_levels"
    
    if (layout == "ib") {
      if(current_tab == "exact" && "ib_match_type" %in% names(filtered_data)) {
        filtered_data <- filtered_data[filtered_data$ib_match_type == "Exact Match", ]
      } else if(current_tab == "over" && "ib_match_type" %in% names(filtered_data)) {
        filtered_data <- filtered_data[filtered_data$ib_match_type == "Overmatch", ]
      } else if(current_tab == "under" && "ib_match_type" %in% names(filtered_data)) {
        filtered_data <- filtered_data[filtered_data$ib_match_type == "Under Match", ]
      } else if(current_tab == "all" && "ib_match_type" %in% names(filtered_data)) {
        filtered_data <- filtered_data[filtered_data$ib_match_type %in% c("Exact Match", "Overmatch", "Under Match"), ]
      }
    } else {
      if(current_tab == "exact" && "match_type" %in% names(filtered_data)) {
        filtered_data <- filtered_data[filtered_data$match_type == "Exact Match", ]
      } else if(current_tab == "over" && "match_type" %in% names(filtered_data)) {
        filtered_data <- filtered_data[filtered_data$match_type %in% c("Good Match", "Overmatch"), ]
      } else if(current_tab == "under" && "match_type" %in% names(filtered_data)) {
        filtered_data <- filtered_data[filtered_data$match_type == "Under Match", ]
      } else if(current_tab == "all" && "match_type" %in% names(filtered_data)) {
        filtered_data <- filtered_data[filtered_data$match_type %in% c("Exact Match", "Good Match", "Overmatch", "No Data"), ]
      }
    }
    
    # ...existing sorting and limiting code...
    if(!is.null(input$sort_by)) {
      if(input$sort_by == "alpha") {
        filtered_data <- filtered_data[order(filtered_data$title), ]
      } else if(input$sort_by == "grade") {
        filtered_data <- filtered_data[order(-filtered_data$grade_score, na.last = TRUE), ]
      } else if(input$sort_by == "distance") {
        filtered_data <- filtered_data[order(filtered_data$distance_miles, na.last = TRUE), ]
      } else if(input$sort_by == "match") {
        if(any(!is.na(filtered_data$match_priority))) {
          filtered_data <- filtered_data[order(filtered_data$match_priority, -filtered_data$grade_score, na.last = TRUE), ]
        }
      }
    }
    
    num_to_show <- if(input$num_courses == "all") {
      nrow(filtered_data)
    } else {
      min(as.numeric(input$num_courses), nrow(filtered_data))
    }
    
    if(nrow(filtered_data) == 0) {
      return(data.frame())
    }
    
    return(head(filtered_data, num_to_show))
  })
  
  output$course_cards <- renderUI({
    layout <- input$qual_layout
    if (is.null(layout)) layout <- "a_levels"
    courses_to_show <- displayed_courses()
    
    if(nrow(courses_to_show) == 0) {
      return(div(style = "text-align: center; color: #b3b3b3; padding: 60px; font-size: 18px;",
                 h3("No courses match your criteria", style = "color: #ffffff; margin-bottom: 16px;"),
                 p("Try adjusting your filters or selections")))
    }
    
    course_cards <- lapply(1:nrow(courses_to_show), function(i) {
      course <- courses_to_show[i, ]
      # --- MATCH BADGE LOGIC ---
      if (layout == "ib") {
        match_label <- if(!is.null(course$ib_match_type)) course$ib_match_type else ""
        badge_class <- switch(match_label,
                              "Exact Match" = "match-exact",
                              "Overmatch" = "match-over",
                              "Under Match" = "match-no",
                              "")
        diff <- if(!is.null(course$ib_score_diff)) course$ib_score_diff else NA
        match_score <- ""
        if(!is.na(diff) && match_label != "") {
          if(diff > 0) match_score <- paste0(" (", diff, " over)")
          else if(diff < 0) match_score <- paste0(" (", abs(diff), " under)")
          else match_score <- " (exact)"
        }
        match_badge <- if(match_label != "") div(class = paste("match-badge", badge_class), paste0(match_label, match_score)) else NULL
      } else {
        match_label <- if(!is.null(course$match_type)) course$match_type else ""
        badge_class <- switch(match_label,
                              "Exact Match" = "match-exact",
                              "Good Match" = "match-good", 
                              "Overmatch" = "match-over",
                              "Under Match" = "match-no",
                              "No Match" = "match-no",
                              "")
        diff <- if(!is.null(course$score_diff)) course$score_diff else NA
        match_score <- ""
        if(!is.na(diff) && match_label != "") {
          if(diff > 0) match_score <- paste0(" (", round(diff/8, 1), " grades over)")
          else if(diff < 0) match_score <- paste0(" (", abs(round(diff/8, 1)), " grades under)")
          else match_score <- " (exact)"
        }
        match_badge <- if(match_label != "") div(class = paste("match-badge", badge_class), paste0(match_label, match_score)) else NULL
      }
      # --- END MATCH BADGE LOGIC ---
      
      university_name <- if(!is.null(course$university_name) && !is.na(course$university_name) && course$university_name != "") {
        course$university_name
      } else {
        "University"
      }
      distance_action_badge <- if(!is.null(course$has_distance) && course$has_distance == TRUE && 
                                  !is.null(course$distance_miles) && !is.na(course$distance_miles)) {
        span(class = "distance-badge", paste(round(course$distance_miles, 1), "miles"))
      } else {
        NULL
      }
      if (layout == "ib") {
        grade_req_text <- if(!is.null(course$ib_grade_req) && !is.na(course$ib_grade_req) && course$ib_grade_req != "") {
          course$ib_grade_req
        } else {
          "N/A"
        }
      } else {
        grade_req_text <- if(!is.null(course$a_level_grade_req) && !is.na(course$a_level_grade_req) && course$a_level_grade_req != "") {
          course$a_level_grade_req
        } else {
          "N/A"
        }
      }
      course_details <- paste(university_name, "•", course$degree_type, "• Grade Req:", grade_req_text)
      div(class = "course-card",
          match_badge,
          div(class = "course-title", 
              onclick = paste0("openModal(", i, ")"),
              course$title),
          div(class = "course-details", 
              course_details),
          div(class = "course-actions",
              tags$button("Course URL", class = "course-btn", 
                          onclick = paste0("window.open('", course$url, "', '_blank')")),
              tags$button("Learn More", class = "course-btn primary",
                          onclick = paste0("openModal(", i, ")")),
              distance_action_badge
          )
      )
    })
    course_cards
  })
  
  
  
  observeEvent(input$selected_course_index, {
    
    if(!is.null(input$selected_course_index) && input$selected_course_index > 0) {
      # Use the same dataset that was used to display the course cards
      current_data <- displayed_courses()
      
      # Make sure the selected index is within range
      if(input$selected_course_index <= nrow(current_data)) {
        # Get course from the displayed dataset
        course <- current_data[input$selected_course_index, ]
        
        # Format salary with proper handling of NA values
        salary_text <- if(!is.na(course$median_salary) && course$median_salary > 0) {
          paste("£", format(course$median_salary, big.mark = ",", scientific = FALSE))
        } else {
          "N/A"
        }
        
        # Format offer rate - UPDATED COLUMN NAME
        offer_rate_text <- if(!is.null(course$offer_rate) && !is.na(course$offer_rate) && course$offer_rate != "") {
          if(is.numeric(course$offer_rate)) {
            paste0(round(course$offer_rate, 1), "%")
          } else {
            course$offer_rate
          }
        } else {
          "Data Not Available"
        }
        
        # Get university name - UPDATED COLUMN NAME
        university_name <- if(!is.null(course$university_name) && !is.na(course$university_name) && course$university_name != "") {
          course$university_name
        } else {
          "University"
        }
        
        # Distance text for modal - only if user has entered postcode and distance is available
        distance_text <- if(!is.null(course$has_distance) && course$has_distance == TRUE && 
                            !is.null(course$distance_miles) && !is.na(course$distance_miles)) {
          paste("Distance:", round(course$distance_miles, 1), "miles")
        } else {
          NULL
        }
        
        # Get match type and subject requirements
        match_type <- if(!is.null(course$match_type)) course$match_type else "No Data"
        subject_requirements_met <- if(!is.null(course$subject_requirements_met)) course$subject_requirements_met else FALSE
        
        # Get selected subjects for highlighting
        selected_subjects <- current_selected_subjects()
        
        # Check if user has selected any subjects
        has_subject_selection <- length(selected_subjects) > 0
        
        # Build requirements text from real data - UPDATED COLUMN NAMES with DEBUG
        # Build requirements text and grade req based on selected tab
        layout <- input$qual_layout
        if (is.null(layout)) layout <- "a_levels"
        
        if (layout == "ib") {
          # IB requirements
          grade_req_text <- if(!is.null(course$ib_grade_req) && !is.na(course$ib_grade_req) && course$ib_grade_req != "") {
            course$ib_grade_req
          } else {
            "N/A"
          }
          requirements_text <- if(!is.null(course$ib_subject_req) && !is.na(course$ib_subject_req) && course$ib_subject_req != "") {
            course$ib_subject_req
          } else {
            "Requirements information not available."
          }
        } else {
          # A Level or Mix requirements
          grade_req_text <- if(!is.null(course$a_level_grade_req) && !is.na(course$a_level_grade_req) && course$a_level_grade_req != "") {
            course$a_level_grade_req
          } else {
            "N/A"
          }
          requirements_text <- if(!is.null(course$a_level_subject_reqs) && !is.na(course$a_level_subject_reqs) && course$a_level_subject_reqs != "") {
            course$a_level_subject_reqs
          } else {
            "Requirements information not available."
          }
        }
        # Add placement year info if available - UPDATED COLUMN NAME
        placement_text <- ""
        if(!is.null(course$placement_year) && !is.na(course$placement_year)) {
          if(course$placement_year == "Yes" || course$placement_year == TRUE) {
            placement_text <- "Placement year available."
          }
        }
        
        # Add year abroad info if available - UPDATED COLUMN NAME
        abroad_text <- ""
        if(!is.null(course$year_abroad) && !is.na(course$year_abroad)) {
          if(course$year_abroad == "Yes" || course$year_abroad == TRUE) {
            abroad_text <- "Year abroad available."
          }
        }
        
        # Add foundation year info if available
        foundation_text <- ""
        if(!is.null(course$foundation_year_available) && !is.na(course$foundation_year_available)) {
          if(course$foundation_year_available == "Yes" || course$foundation_year_available == TRUE) {
            foundation_text <- "Foundation year available."
          }
        }
        
        # Combine all requirements text
        additional_options <- c(placement_text, abroad_text, foundation_text)
        additional_options <- additional_options[additional_options != ""]
        
        if(length(additional_options) > 0) {
          if(requirements_text != "") {
            requirements_text <- paste(requirements_text, paste(additional_options, collapse = " "), sep = " ")
          } else {
            requirements_text <- paste(additional_options, collapse = " ")
          }
        }
        
        # Fallback if no requirements data
        if(requirements_text == "") {
          requirements_text <- "Requirements information not available."
        }
        
        cat("DEBUG MODAL: Final requirements text:", requirements_text, "\n")
        cat("DEBUG MODAL: Current selected subjects:", paste(selected_subjects, collapse = ", "), "\n")
        
        # Find matched subjects using YOUR working logic with DEBUG
        matched_subjects_list <- if(has_subject_selection && requirements_text != "" && requirements_text != "Requirements information not available.") {
          result <- find_matched_subjects(selected_subjects, requirements_text)
          cat("DEBUG MODAL: find_matched_subjects returned:", paste(result, collapse = ", "), "\n")
          result
        } else {
          cat("DEBUG MODAL: Skipping subject matching - no subjects or requirements\n")
          character(0)
        }
        
        # Find similar courses from the full dataset (not just displayed courses)
        cache_key <- paste(course$title, input$qual_layout, sep = "_")
        if (!is.null(similar_courses_cache[[cache_key]])) {
          similar_courses <- similar_courses_cache[[cache_key]]
        } else {
          similar_courses <- find_similar_courses(course, degree_data, limit = 3)
          similar_courses_cache[[cache_key]] <- similar_courses
        }
        layout <- input$qual_layout
        if (is.null(layout)) layout <- "a_levels"
        if (layout == "ib") {
          match_type <- if(!is.null(course$ib_match_type)) course$ib_match_type else "No Data"
        } else {
          match_type <- if(!is.null(course$match_type)) course$match_type else "No Data"
        }
        
        # Update modal content with CLEAN requirements and distance
        session$sendCustomMessage("updateModal", list(
          grade_type = if (input$qual_layout == "ib") "ib" else "alevel",
          degree_type = course$degree_type,
          title = course$title,
          university = university_name,
          grade_req = paste("Grade Req:", grade_req_text),
          requirements = requirements_text,
          salary = salary_text,
          offer_rate = offer_rate_text,
          url = course$url,
          match_type = match_type,
          subject_requirements_met = subject_requirements_met,
          has_subject_selection = has_subject_selection,
          requirements = requirements_text,
          selected_subjects = selected_subjects,
          matched_subjects = if(length(matched_subjects_list) > 0) matched_subjects_list else NULL,
          distance_text = distance_text,
          similar_courses = if(nrow(similar_courses) > 0) {
            lapply(1:nrow(similar_courses), function(i) {
              sim_course <- similar_courses[i, ]
              sim_university <- if(!is.null(sim_course$university_name) && !is.na(sim_course$university_name) && sim_course$university_name != "") {
                sim_course$university_name
              } else {
                "University"
              }
              # Find index in displayed_courses()
              sim_index <- which(displayed_courses()$title == sim_course$title)[1]
              list(
                title = sim_course$title,
                degree_type = sim_course$degree_type,
                university = sim_university,
                grade_req = if (input$qual_layout == "ib") sim_course$ib_grade_req else sim_course$a_level_grade_req,
                url = sim_course$url,
                index = sim_index
              )
            })
          } else {
            list()
          }
        ))
        
      }
    }
  })
  
  observeEvent(input$qual_layout, {
    # If modal is open, re-trigger the modal update for the current course
    if(!is.null(input$selected_course_index) && input$selected_course_index > 0) {
      current_data <- displayed_courses()
      if(input$selected_course_index <= nrow(current_data)) {
        course <- current_data[input$selected_course_index, ]
        # Copy the modal update logic from observeEvent(input$selected_course_index, ...) here
        # (You can refactor this into a helper function to avoid duplication)
        # --- BEGIN MODAL UPDATE LOGIC ---
        salary_text <- if(!is.na(course$median_salary) && course$median_salary > 0) {
          paste("£", format(course$median_salary, big.mark = ",", scientific = FALSE))
        } else {
          "N/A"
        }
        offer_rate_text <- if(!is.null(course$offer_rate) && !is.na(course$offer_rate) && course$offer_rate != "") {
          if(is.numeric(course$offer_rate)) paste0(round(course$offer_rate, 1), "%") else course$offer_rate
        } else {
          "Data Not Available"
        }
        university_name <- if(!is.null(course$university_name) && !is.na(course$university_name) && course$university_name != "") {
          course$university_name
        } else {
          "University"
        }
        distance_text <- if(!is.null(course$has_distance) && course$has_distance == TRUE && 
                            !is.null(course$distance_miles) && !is.na(course$distance_miles)) {
          paste("Distance:", round(course$distance_miles, 1), "miles")
        } else {
          NULL
        }
        match_type <- if(!is.null(course$match_type)) course$match_type else "No Data"
        subject_requirements_met <- if(!is.null(course$subject_requirements_met)) course$subject_requirements_met else FALSE
        selected_subjects <- current_selected_subjects()
        has_subject_selection <- length(selected_subjects) > 0
        layout <- input$qual_layout
        if (is.null(layout)) layout <- "a_levels"
        if (layout == "ib") {
          grade_req_text <- if(!is.null(course$ib_grade_req) && !is.na(course$ib_grade_req) && course$ib_grade_req != "") {
            course$ib_grade_req
          } else {
            "N/A"
          }
          requirements_text <- if(!is.null(course$ib_subject_req) && !is.na(course$ib_subject_req) && course$ib_subject_req != "") {
            course$ib_subject_req
          } else {
            "Requirements information not available."
          }
        } else {
          grade_req_text <- if(!is.null(course$a_level_grade_req) && !is.na(course$a_level_grade_req) && course$a_level_grade_req != "") {
            course$a_level_grade_req
          } else {
            "N/A"
          }
          requirements_text <- if(!is.null(course$a_level_subject_reqs) && !is.na(course$a_level_subject_reqs) && course$a_level_subject_reqs != "") {
            course$a_level_subject_reqs
          } else {
            "Requirements information not available."
          }
        }
        placement_text <- ""
        if(!is.null(course$placement_year) && !is.na(course$placement_year)) {
          if(course$placement_year == "Yes" || course$placement_year == TRUE) {
            placement_text <- "Placement year available."
          }
        }
        abroad_text <- ""
        if(!is.null(course$year_abroad) && !is.na(course$year_abroad)) {
          if(course$year_abroad == "Yes" || course$year_abroad == TRUE) {
            abroad_text <- "Year abroad available."
          }
        }
        foundation_text <- ""
        if(!is.null(course$foundation_year_available) && !is.na(course$foundation_year_available)) {
          if(course$foundation_year_available == "Yes" || course$foundation_year_available == TRUE) {
            foundation_text <- "Foundation year available."
          }
        }
        additional_options <- c(placement_text, abroad_text, foundation_text)
        additional_options <- additional_options[additional_options != ""]
        if(length(additional_options) > 0) {
          if(requirements_text != "") {
            requirements_text <- paste(requirements_text, paste(additional_options, collapse = " "), sep = " ")
          } else {
            requirements_text <- paste(additional_options, collapse = " ")
          }
        }
        if(requirements_text == "") {
          requirements_text <- "Requirements information not available."
        }
        matched_subjects_list <- if(has_subject_selection && requirements_text != "" && requirements_text != "Requirements information not available.") {
          result <- find_matched_subjects(selected_subjects, requirements_text)
          result
        } else {
          character(0)
        }
        cache_key <- paste(course$title, input$qual_layout, sep = "_")
        if (!is.null(similar_courses_cache[[cache_key]])) {
          similar_courses <- similar_courses_cache[[cache_key]]
        } else {
          similar_courses <- find_similar_courses(course, degree_data, limit = 3)
          similar_courses_cache[[cache_key]] <- similar_courses
        }
        session$sendCustomMessage("updateModal", list(
          grade_type = if (input$qual_layout == "ib") "ib" else "alevel",   
          degree_type = course$degree_type,
          title = course$title,
          university = university_name,
          grade_req = paste("Grade Req:", grade_req_text),
          requirements = requirements_text,
          salary = salary_text,
          offer_rate = offer_rate_text,
          url = course$url,
          match_type = match_type,
          subject_requirements_met = subject_requirements_met,
          has_subject_selection = has_subject_selection,
          requirements = requirements_text,
          selected_subjects = selected_subjects,
          matched_subjects = if(length(matched_subjects_list) > 0) matched_subjects_list else NULL,
          distance_text = distance_text,
          similar_courses = if(nrow(similar_courses) > 0) {
            lapply(1:nrow(similar_courses), function(i) {
              sim_course <- similar_courses[i, ]
              sim_university <- if(!is.null(sim_course$university_name) && !is.na(sim_course$university_name) && sim_course$university_name != "") {
                sim_course$university_name
              } else {
                "University"
              }
              # Find index in displayed_courses()
              sim_index <- which(displayed_courses()$title == sim_course$title)[1]
              list(
                title = sim_course$title,
                degree_type = sim_course$degree_type,
                university = sim_university,
                grade_req = if (input$qual_layout == "ib") sim_course$ib_grade_req else sim_course$a_level_grade_req,
                url = sim_course$url,
                index = sim_index
              )
            })
          } else {
            list()
          }
        ))
        # --- END MODAL UPDATE LOGIC ---
      }
    }
  }, ignoreInit = TRUE)
  
  # Observe sort/filter changes to preserve scroll position - OPTIMIZED
  observeEvent(c(input$sort_by, input$num_courses, input$current_tab), {
    # Only trigger when there are actual courses to display
    if(input$submit_filters > 0 || nrow(degree_data) > 0) {
      session$sendCustomMessage("preserveScroll", list())
    }
  }, ignoreInit = TRUE)
  observeEvent(current_selected_subjects(), {
    cat("Subjects changed to:", paste(current_selected_subjects(), collapse = ", "), "\n")
    
    # Check if a modal is currently open and re-trigger the modal update
    if(!is.null(input$selected_course_index) && input$selected_course_index > 0) {
      cat("Modal is open, updating subject comparison\n")
      
      # Get current modal course data
      current_data <- displayed_courses()
      if(input$selected_course_index <= nrow(current_data)) {
        # Re-trigger the modal update with new subject matching
        course <- current_data[input$selected_course_index, ]
        
        # Get current selected subjects
        selected_subjects <- current_selected_subjects()
        has_subject_selection <- length(selected_subjects) > 0
        
        cat("Current subjects:", paste(selected_subjects, collapse = ", "), "\n")
        
        # Build requirements text from real data - UPDATED COLUMN NAMES with DEBUG
        requirements_text <- ""
        
        # Add A-level subjects (main requirement)
        if(!is.null(course$a_level_subject_reqs) && !is.na(course$a_level_subject_reqs) && course$a_level_subject_reqs != "") {
          requirements_text <- course$a_level_subject_reqs
          cat("DEBUG UPDATE: Found a_level_subject_reqs:", course$a_level_subject_reqs, "\n")
        }
        
        # Add placement year info if available
        placement_text <- ""
        if(!is.null(course$placement_year) && !is.na(course$placement_year)) {
          if(course$placement_year == "Yes" || course$placement_year == TRUE) {
            placement_text <- "Placement year available."
          }
        }
        
        # Add year abroad info if available  
        abroad_text <- ""
        if(!is.null(course$year_abroad) && !is.na(course$year_abroad)) {
          if(course$year_abroad == "Yes" || course$year_abroad == TRUE) {
            abroad_text <- "Year abroad available."
          }
        }
        
        # Add foundation year info if available
        foundation_text <- ""
        if(!is.null(course$foundation_year_available) && !is.na(course$foundation_year_available)) {
          if(course$foundation_year_available == "Yes" || course$foundation_year_available == TRUE) {
            foundation_text <- "Foundation year available."
          }
        }
        
        # Combine all requirements text
        additional_options <- c(placement_text, abroad_text, foundation_text)
        additional_options <- additional_options[additional_options != ""]
        
        if(length(additional_options) > 0) {
          if(requirements_text != "") {
            requirements_text <- paste(requirements_text, paste(additional_options, collapse = " "), sep = " ")
          } else {
            requirements_text <- paste(additional_options, collapse = " ")
          }
        }
        
        # Fallback if no requirements data
        if(requirements_text == "") {
          requirements_text <- "Requirements information not available."
        }
        
        cat("DEBUG UPDATE: Requirements text:", requirements_text, "\n")
        cat("DEBUG UPDATE: Current subjects:", paste(selected_subjects, collapse = ", "), "\n")
        
        # Find matched subjects using YOUR working logic with DEBUG
        matched_subjects_list <- if(has_subject_selection && requirements_text != "" && requirements_text != "Requirements information not available.") {
          result <- find_matched_subjects(selected_subjects, requirements_text)
          cat("DEBUG UPDATE: find_matched_subjects returned:", paste(result, collapse = ", "), "\n")
          result
        } else {
          cat("DEBUG UPDATE: Skipping subject matching - no subjects or requirements\n")
          character(0)
        }
        
        cat("Matched subjects:", paste(matched_subjects_list, collapse = ", "), "\n")
        
        # Update only the subject comparison part of the modal
        session$sendCustomMessage("updateSubjectComparison", list(
          has_subject_selection = has_subject_selection,
          selected_subjects = selected_subjects,
          matched_subjects = if(length(matched_subjects_list) > 0) matched_subjects_list else NULL
        ))
      }
    }
  }, ignoreInit = TRUE)
}

# Run the application
shinyApp(ui = ui, server = server, options = list(height = 1080))