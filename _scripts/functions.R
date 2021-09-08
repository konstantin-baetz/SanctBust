sanct_load <- function(s = "full", p = F){
  if (s == "full"){
    if (p == F){
      load("C:/Users/konst/Google Drive/Studium/Dissertation/paper3/_data/EUSANCT/UN_Sanct2")
      load("C:/Users/konst/Google Drive/Studium/Dissertation/paper3/_data/EUSANCT/US_Sanct2")
      load("C:/Users/konst/Google Drive/Studium/Dissertation/paper3/_data/EUSANCT/EU_Sanct2")
      Sanctions <- rbind(EU_Sanct, UN_Sanct, US_Sanct)
    } else if (p == T){
      #run potential sanction cases (can also be combinded with option "seperately")
      load("C:/Users/konst/Google Drive/Studium/Dissertation/paper3/_data/EUSANCT/EU_Sanct_potential")
      load("C:/Users/konst/Google Drive/Studium/Dissertation/paper3/_data/EUSANCT/US_Sanct_potential")
      load("C:/Users/konst/Google Drive/Studium/Dissertation/paper3/_data/EUSANCT/UN_Sanct_potential")
      Sanctions <- rbind(EU_Sanct, UN_Sanct, US_Sanct)
    }
  } else if (s == "US"){
    if (p == F){
      load("C:/Users/konst/Google Drive/Studium/Dissertation/paper3/_data/EUSANCT/US_Sanct2")
      Sanctions <- US_Sanct
    } else if (p == T) {
      load("C:/Users/konst/Google Drive/Studium/Dissertation/paper3/_data/EUSANCT/US_Sanct_potential")
      Sanctions <- US_Sanct
    }
  } else if (s == "EU"){
    if (p == F){
      load("C:/Users/konst/Google Drive/Studium/Dissertation/paper3/_data/EUSANCT/EU_Sanct2")
      Sanctions <- EU_Sanct
    } else if (p == T){
      load("C:/Users/konst/Google Drive/Studium/Dissertation/paper3/_data/EUSANCT/EU_Sanct_potential")
      Sanctions <- EU_Sanct
    }
  } else if (s == "UN"){
    if (p == F){
      load("C:/Users/konst/Google Drive/Studium/Dissertation/paper3/_data/EUSANCT/UN_Sanct2")
      Sanctions <- UN_Sanct
    } else if (p == T){
      load("C:/Users/konst/Google Drive/Studium/Dissertation/paper3/_data/EUSANCT/UN_Sanct_potential")
      Sanctions <- UN_Sanct
    }
  }
  return(Sanctions)
  #names(Sanctions)
}