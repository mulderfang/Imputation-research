# Missing value process

## 1. Stratify
 - strata.LH (neyman allocation)
 - strata.LH (proportion allocation)
 - samplingstrata

## 2. Imputation
  - 平均值插補法(mean)：以抽樣後各層的平均數來插補該層的遺失值。
  - 中位數插補法(median)：以抽樣後各層的中位數來插補該層的遺失值。
  - 熱卡插補法(hotdeck)：在抽樣後，依照各層輔助變數，找尋其他未遺失資料輔助變數和遺失值輔助變數類似的資料，以其觀察值來插補。
  - K 最近鄰插補法(knn)：以 k 筆最鄰近遺失值資料的觀察值平均數做插補。
  - 貝式迴歸插補法(norm)：以分層後的各層作貝氏迴歸，以預測的值來插補遺失值。
  - 預測均值配對插補法(pmm)：以分層後的各層作貝氏迴歸，預測所有資料的值，找尋和遺失值預測值相近的其他未遺失資料預測值，從中隨機挑出一筆未遺失資料，以其觀察值作插補。
  
## 3. Evaluation 
  - MSE
  - KS test
  - Delta
  
## 4. Bast imputation method
  - histogram
  - descriptive statistics
