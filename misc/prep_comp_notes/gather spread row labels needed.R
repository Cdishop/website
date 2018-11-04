cd_try <- data.frame(
  'b_partial' = c(1,2,3),
  'b_wo_partial' = c(4,5,6),
  'se_partial' = c(6,7,8),
  'se_wo_partial' = c(3,2,1)
)

cd_try

cd_try <- cd_try %>%
  gather(b_partial, b_wo_partial, key = 'model', value = 'b1') 

cd_try

cd_try <- cd_try %>%
  gather(se_partial, se_wo_partial, key = 'se_model', value = 'sd')

cd_try

# Nope

# I need to gather, split, and then spread

cd_try <- data.frame(
  'b_partial' = c(1,2,3),
  'b_wo_partial' = c(4,5,6),
  'se_partial' = c(6,7,8),
  'se_wo_partial' = c(3,2,1)
)

cd_try

cd_try <- cd_try %>%
  gather(b_partial, b_wo_partial, 
         se_partial, se_wo_partial,
         key = 'result_model', value = 'value')

cd_try

cd_try <- cd_try %>%
  separate(result_model, into = c('result', 'model'), sep = "_")

cd_try

cd_try <- cd_try %>%
  mutate(row_help = rep(1:6, 2))

cd_try <- cd_try %>%
  spread(result, value)

cd_try
