all: index 160512_run1_calibration 160519_run2_calibration 160602_run3_calibration 20170321_run4_calibration 20171107_run5_calibration 20171109_run6_calibration 20171109_run7_calibration 20180725_run8_calibration 20180727_run9_calibration 20180731_run10_calibration 2018_2015_combinedrunspostcalibration

index:
	Rscript -e "rmarkdown::render('index.Rmd', output_dir = 'documents')"
	
160512_run1_calibration:
	Rscript -e "rmarkdown::render('160512_run1_calibration.Rmd', output_dir = 'documents')"
	
160519_run2_calibration:
		Rscript -e "rmarkdown::render('160519_run2_calibration.Rmd', output_dir = 'documents')"

160602_run3_calibration:
		Rscript -e "rmarkdown::render('160602_run3_calibration.Rmd', output_dir = 'documents')"

20170321_run4_calibration:
		Rscript -e "rmarkdown::render('20170321_run4_calibration.Rmd', output_dir = 'documents')"

20171107_run5_calibration:
	Rscript -e "rmarkdown::render('20171107_run5_calibration.Rmd', output_dir = 'documents')"

20171109_run6_calibration:
	Rscript -e "rmarkdown::render('20171109_run6_calibration.Rmd', output_dir = 'documents')"

20171109_run7_calibration:
	Rscript -e "rmarkdown::render('20171109_run7_calibration.Rmd', output_dir = 'documents')"

20180725_run8_calibration:
	Rscript -e "rmarkdown::render('20180725_run8_calibration.Rmd', output_dir = 'documents')"
	
20180727_run9_calibration:
	Rscript -e "rmarkdown::render('20180727_run9_calibration.Rmd', output_dir = 'documents')"
	
20180731_run10_calibration:
	Rscript -e "rmarkdown::render('20180731_run10_calibration.Rmd', output_dir = 'documents')"
	
2018_2015_combinedrunspostcalibration:
	Rscript -e "rmarkdown::render('2018_2015_combinedrunspostcalibration.Rmd', output_dir = 'documents')"