# Impact of Yield Curve Control Policy on Financial Market Expectations

This repo contains scripts from the Yield Curve Control (YCC) project. The project investigates the impact of YCC policy announcements on financial market expectations using high frequency data.

- **pre_BVAR_part/** collects short R scripts that process data and make it ready for Bayesian VAR estimation.
- **matlab/** contains MATLAB codes that run the Bayesian VAR estimation via Gibbs sampling, a monte carlo simulator that extracts structural shocks via sign restrictions, and an irf plot generator.
- **post_BVAR_part/** contains R scripts that query data from the estimations, process, run regression analysis, and report results.
- **graphing/** contains ggplot scripts for visualizing results
- **data/** contains processed data files used in the estimation, available upon request

[Click for draft](https://drive.google.com/file/d/1TJVIUvYJ6dd44ahKl28UGGoc3aHNdYLE/view?usp=share_link)

***Abstract:***  I examine how a fixed-rate purchase policy, or the yield curve control (YCC), performs better than traditional fixed-quantity QE projects in managing market expectations using evidence from Japan. Japan has the longest history of implementing this novel policy. Using the high-frequency identification method, I captured the monetary policy surprises around the Bank of Japan’s policy meetings in the past two decades. I implemented a customized Bayesian vector autoregression model and decomposed the raw surprises into policy specific and macroeconomic information elements via sign restrictions. My analysis on the decomposed policy surprises shows that YCC reduces market uncertainty and aligns market policy expectations, especially when the control band is sufficiently narrow. This effect extends from the targeted asset, 10-year Japanese government bond, to the shorter end of the yield curve and to other adjacent asset markets, albeit with diminishing magnitude, depending on their distance from the targeted asset in terms of both maturity and asset type. The effect weakens and uncertainty increases as the control band widens. The effect is monetary specific. It doesn’t affect market’s expectation on macroeconomic conditions. My findings suggest that YCC has the potential to be a viable policy option, with the set up of target control band being a crucial consideration in policy design.