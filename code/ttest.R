ttest <- merge_complete |> 
  mutate(across(PRIME_FARMLAND_IF_DRAINED:PRIME_FARMLAND_IF_IRRIGATED,
                ~ case_when(
                  .x > 0 ~ 1,
                  TRUE ~ 0
                )))

drain_irr <- t.test(ttest[ttest$PRIME_FARMLAND_IF_DRAINED==1,1], 
       ttest[ttest$PRIME_FARMLAND_IF_IRRIGATED==1,1])

prime_drain <- t.test(ttest[ttest$ALL_AREAS_ARE_PRIME_FARMLAND==1,1],
       ttest[ttest$PRIME_FARMLAND_IF_DRAINED==1,1])

prime_irr <- t.test(ttest[ttest$ALL_AREAS_ARE_PRIME_FARMLAND==1,1],
       ttest[ttest$PRIME_FARMLAND_IF_IRRIGATED==1,1])