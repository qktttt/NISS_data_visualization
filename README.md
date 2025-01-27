**[Link to the visulization portal](https://best-data-visualization.shinyapps.io/niss_visualization/)**

**Teammates: Mingshi Cui (Rutgers University), Litong Zhang (formerly at Texas A&M University)**

# Report on the Data Visualization

## 1. Topic of the Visualization
This visualization explores the relationship between **gender, educational attainment, and annual earnings** in the United States from 1990 to 2019. It focuses on income difference between males and females and the impact of different levels of education on earnings.

---

## 2. Implementation of the Interactive Data Visualization Portal
The portal was created using **R-shiny** and includes the following features and functionalities:

### Accessibility Features:
- **Colorblind-Friendly Design**:
  - Avoids problematic color combinations, such as red-green or blue-purple, to ensure accessibility for users with Deuteranopia, Protaniopia, or Tritanopia.
  - Uses warm, high-contrast color palettes to improve visibility.

### Interactive Capabilities:
- Users can interact with plots to:
  - Click specific data points to view annotations and detailed statistics.
  - Access summary statistics, such as standard errors, earnings comparisons, or growth rates, depending on the selected plot.
  - The portal can automatically summarizes and describes the key insights of the currently selected visualization scope in clear sentences, making it easier for users to understand the data.
- In addition to the main page, there are also additional interactive pages:
  - **"Compare By Yourself"** section: Allows users to compare earnings by gender and education level at their own interest.
  - **"Your Earnings Really Grown"** section: Visualizes inflation-adjusted trends and earnings growth over time.

### Design Limitations:
1. **Resolution Compatibility**:
   - The portal is optimized for 1920x1080 screen resolution. Other resolutions may result in disorganized layouts.
   - The portal is not fully compatible with mobile devices.

2. **Trend Visibility**:
   - Trends in histogram plots (e.g., number of full-time workers or percentage growth) are sometimes hard to detect due to subtle changes in the data.
   - A "standard error" button is available to display error bars, but sometimes, depends on data value, ther are small and difficult to notice.

---

## 3. Major Findings
### Gender Disparities:
- Males consistently earn more than females at all education levels.
- The largest gender gap occurred in 1990, with males earning $18,310 more than females (adjusted to 2019 dollars).
- The income gap has decreased slightly but remains significant.

### Educational Attainment:
- Higher education levels lead to higher earnings.
- College serves as a turning point where earnings increase significantly.
- Males with associate's degrees often earn more than females with bachelor's degrees, highlighting persistent gender inequalities.

### Inflation-Adjusted Earnings:
- While earnings in current dollars have increased, inflation-adjusted earnings have remained similar or decreased for lower education levels.
- Higher education groups (e.g., master's or professional degrees) show real income growth even after accounting for inflation.

---
