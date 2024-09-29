# library(devtools)
# c(
#   'CaoBittencourt' = 'atlas.labor'
# ) -> git_pkgs
#
# Map(
#   function(git, profile){
#
#     if(!require(git, character.only = T)){
#
#       install_github(
#         paste0(profile, '/', git)
#         , upgrade = F
#         , force = T
#       )
#
#     }
#
#     require(git, character.only = T)
#
#   }
#   , git = git_pkgs
#   , profile = names(git_pkgs)
# )