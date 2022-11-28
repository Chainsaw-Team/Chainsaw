% read .mat file exported by Simulink model in 'time series' format
function data = getSimulinkToFile(path)

    data = load(path).ans.Data;
    data = squeeze(data);
