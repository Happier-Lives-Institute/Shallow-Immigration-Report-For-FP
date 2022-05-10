#1. Years left for parents = 40, children = 65. 
# Life satisfaction of USA = 7/10, Afganistan = 3/10. 
# Assumption: A.) Likelihood they migrate to another country = 0. 
# B.) They stay there for the rest of their lives. 
# C.) These wellbeing levels persis throughout their lives. 

ParentWELLBYs = (7 - 3) * 40 * 2          # Parents wellbeing gained
ChildrenWELLBYs = (7 - 3) * 65 * 2        # The two childrens wellbeing gained 
migrationWELLBYs = ParentWELLBYs + ChildrenWELLBYs # Family wellbeing gained 
migrationWELLBYs 

# Total WELLBYS from $1000 CT 
CT_WELLBYs = 1 * 5 * 2 # SD impact on WELLBEING * people affected * SD of lifesatisfaction 

# Equivalence to CTs. 
(migrationWELLBYs / CT_WELLBYs) * 1000

#2. Same as #1 except we assume that the wellbeing levels converge by in 40 years. 

migrationWELLBYs = (7 - 3) * 40 * 4 * 0.5 # Family wellbeing gained 
(migrationWELLBYs / CT_WELLBYs) * 1000



#3. Same as #2 except we assume that they'd otherwise go to Pakistan (5/10). 

migrationWELLBYs = (7 - 5) * 40 * 4 * 0.5 # Family wellbeing gained 
(migrationWELLBYs / CT_WELLBYs) * 1000

185 * 2
