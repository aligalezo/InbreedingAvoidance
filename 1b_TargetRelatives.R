## List of target relative classes for analyses
## Ali Galezo

target_relatives <- c("MotherSon","FatherDaughter",
                      "MatSibs","PatSibs",
                      "HalfFirstCousins",
                      "AuntNephew_DadMatSib","AuntNephew_MomMatSib","AuntNephew_MomPatSib","AuntNephew_DadPatSib",
                      "UncleNiece_MomPatSib","UncleNiece_MomMatSib","UncleNiece_DadMatSib","UncleNiece_DadPatSib")
saveRDS(target_relatives, "Data/Processed/target_relatives.Rda")

