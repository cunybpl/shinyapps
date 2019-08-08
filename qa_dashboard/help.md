- `ordering`: a list of models. The models must be listed hierarchically with model that needs to be prioritized at the front of the list and BEMA will only run models listed in `ordering`. For example, below is a usual config object for electricity:
```
{
    'Elec': {
        'ordering': ['5P', '4P', '3PC', '2P'],
        'r2_threshold': 0.75,
        'cv_rmse_threshold': 0.25,
        'ignore_main _test': False
    }
}
```
Note that `'3PH'` is missing in `'ordering'`. Since we usually discard `'3PH'` (heating model) for electiricity, we do not even need to consider running it. However, one has the option to run it and all one has to do is just adding `'3PH'` to the list of models in `'ordering'`. Also note that here `'5P'` (index: 0) model is prioritized over `'4P'` (index: 1) since it is at front of the list and `'4P'` over `'3PC'` and so on.
- `'r2_threshold'`: an interger between (0,1); R-squared threshold.
- `'cv_rmse_threshold'`: an integer between (0,1); CV-RMSE threshold.
- `'ignore_main_test'`: a boolean. If it is set to `True`, while selecting a valid model, it will ignore statistical tests (shape test, data pop test, t-test). In other words, if `'ignore_main_test'` is set to `True`, a model that satisfies the r-squared and CV-RMSE thresholds and has an index in `'ordering'` of zero, but fails one or more of statistical tests, will still be chosen as the best model even if other models both meet the r-squared and CV-RMSE thresholds and pass all of statistical tests.
