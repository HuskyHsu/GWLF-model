var app = new Vue({
    el: '#app',
    data: {
        CN2: 74,
        r: 0.042,
        part: "3",
        Ur: 10,
        U: 0,
        S: 10,
        G: 0,
        data: undefined
    },
    methods: {
        cal: () => {
            GWLF(app);
            plot(app);
        },
        readFile: (e) => {
            reader(e.target.files[0] || e.dataTransfer.files[0])
                .then(function (reader) {
                    app.data = reader;
                })
                .catch(function (error) {
                    console.log(error);
                });
        }
    }
})
