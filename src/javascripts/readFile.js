function reader(file, options) {
	options = options || {};
	return new Promise(function (resolve, reject) {
		let reader = new FileReader();

		reader.onload = function () {

            let lines = reader.result;
            lines = lines.replace(/ /g, '').split('\n'); //去空白分割
            lines.splice(0, 1); //去檔頭

			resolve(lines.map(line => {
                let [day, T, P] = line.split(',');
                return {
                    date: new Date(`${day.substring(0, 4)}/${day.substring(4, 6)}/${day.substring(6, 8)}`), 
                    P: P - 0,
                    T: T - 0
                }
            }));
		};
		reader.onerror = reject;

		if (options.accept && !new RegExp(options.accept).test(file.type)) {
			reject({
				code : 1,
				msg : 'wrong file type'
			});
		}

		if (!file.type || file.type.match(/application\/vnd.ms-excel|application\/csv|text\/csv|text\/comma-separated-values/)) {
			reader.readAsText(file);
		} else {
			reader.readAsDataURL(file);
		}
	});
}