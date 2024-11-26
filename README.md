# AN6003_GenderDiscriminationLawsuit
Used data analytics to defend the school’s stance on no gender discrimination in promotions and salary increases

## Problem Statement  
This project aims to defend Houston College of Medicine against allegations of gender discrimination in promotions and salary settings. Female doctors at the college claimed they were unfairly treated in these areas. Our analysis seeks to demonstrate that these claims are not supported by the data.

## Data Dictionary  
The dataset, sourced from [Kaggle Website](https://www.kaggle.com/datasets/hjmjerry/gender-discrimination/data), includes the following variables:  
- **Dept**: Department *(1 = Biochemistry/Molecular Biology, 2 = Physiology, 3 = Genetics, 4 = Pediatrics, 5 = Medicine, 6 = Surgery)*  
- **Gender**: *(1 = Male, 0 = Female)*  
- **Clin**: Clinical emphasis *(1 = Primarily clinical, 0 = Primarily research)*  
- **Cert**: Board certification *(1 = Certified, 0 = Not certified)*  
- **Prate**: Publication rate *(number of publications divided by years since obtaining MD)*  
- **Exper**: Years since obtaining MD  
- **Rank**: Academic rank *(1 = Assistant Professor, 2 = Associate Professor, 3 = Full Professor)*  
- **Sal94**: Salary in 1994  
- **Sal95**: Salary in 1995  

## Key Findings  
### Promotion  
- **Experience**:  
  Men generally have more years of experience than women. Logistic regression and CART models identified experience as the most significant factor in achieving promotions. Interestingly, women tend to achieve promotions more quickly than men, suggesting a lower promotion threshold in terms of experience for women.  

- **Board Certification**:  
  Board certification significantly enhances the likelihood of attaining Full Professor status. However, the lower board certification rate among women may explain their reduced representation in Full Professor positions.  

- **Publication Rate**:  
  Women have a higher publication rate at the Assistant and Associate Professor levels compared to men. However, their publication rate declines at the Full Professor level, while it increases for men.  

### Salary  
- **Experience and Salary**:  
  Salaries increase with experience, and since men generally have more experience, they tend to earn higher salaries. However, after adjusting for experience, salary gaps between men and women diminish significantly.  

- **Regression Analysis**:  
  Regression analysis indicates that gender is not a significant determinant of salary or salary increments once we control for experience, certification, department, rank, and clinical emphasis. This finding suggests that the observed salary differences are driven by factors other than gender.
  
## Conclusion  
Our analysis indicates that the observed differences in promotions and salary are primarily driven by experience and board certification rather than gender. Therefore, there is no significant evidence of gender discrimination in promotions and salary settings at Houston College of Medicine.
