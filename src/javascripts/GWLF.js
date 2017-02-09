function GWLF(init) {

    const t1 = new Date().getTime();
    const {CN2, r, part, Ur} = init;
    let {data} = init;

    data[0].U = 0;
    data[0].S = 10;
    data[0].G = 0;

    const CN1 = CN2 / (2.3 - 0.013 * CN2);
    const CN3 = CN2 / (0.43 + 0.0057 * CN2);
    const CN32 = CN3 - CN2;
    const CN21 = CN2 - CN1;

    const P5_attr = {
        fallow: { AM1: 1.27, AM2: 2.79 },
        growth: { AM1: 3.56, AM2: 5.33 }
    };

    P5_attr.fallow.AM21 = P5_attr.fallow.AM2 - P5_attr.fallow.AM1;
    P5_attr.growth.AM21 = P5_attr.growth.AM2 - P5_attr.growth.AM1;

    const C = [[0.976,0.84,0.8,0.95],[0.975,0.84,0.79,0.96],[0.984,0.88,0.84,0.97],
              [0.998,0.97,0.91,1],[1.007,1.01,0.95,1],[1.002,0.98,0.92,1],
              [0.98,0.88,0.85,0.98],[0.982,0.9,0.87,0.98],[0.988,0.93,0.89,0.99],
              [1.002,0.99,0.94,0.99],[1.009,1.02,0.96,1.01],[0.998,0.96,0.89,0.98]];

    const H = [[10.65,10.68,10.83,10.8],[11.15,11.18,11.33,11.3],[11.85,11.89,12.03,12],
              [12.6,12.6,12.6,12.6],[13.15,13.12,13.09,13.12],[13.5,13.41,13.3,13.45],
              [13.35,13.33,13.3,13.34],[12.85,12.82,12.8,12.81],[12.1,12.1,12.1,12.1],
              [11.4,11.4,11.4,11.4],[10.8,10.8,10.8,10.8],[10.5,10.57,10.73,10.7]];


    data = data.map((once, i) => {
        const month = once.date.getMonth();

        //-------------------------------------P5:臨前5日降雨量
        once.P5 = 0;
        for(let j = i - 5; j < i; j++){
            once.P5 += j < 0 ? 0 : data[j].P;
        }

        //-------------------------------------CN:地表現況CN值
        {
            let AM1, AM2, AM21;

            //3~11月為生長期，其餘為休耕期
            if (month > 1 && month <= 10) {
                AM1 = P5_attr.growth.AM1;
                AM2 = P5_attr.growth.AM2;
                AM21 = P5_attr.growth.AM21;
            } else {
                AM1 = P5_attr.fallow.AM1;
                AM2 = P5_attr.fallow.AM2;
                AM21 = P5_attr.fallow.AM21;
            }

            if (once.P5 < AM1){
                once.CN = (CN21) / (AM1) * (once.P5) + CN1;
            } else if (once.P5 >= AM1 && once.P5 <= AM2) {
                once.CN = (CN32) / (AM21) * (once.P5 - AM1) + CN2;
            } else {
                once.CN = CN3;
            }
        }
        
        //-------------------------------------Q:地表逕流
        {
            const S = 2540 / once.CN - 25.4
            once.Q = (once.P < 0.2 * S) ? 0 : ((once.P - 0.2 * S)**2) / (once.P + 0.8 * S);
        }
        //-------------------------------------ET:蒸發量
        {
            const e = 0.611 * Math.exp(17.3 * once.T / (once.T + 237.3));
            const PET = (29.8 * H[month][part] * e / (once.T + 273.2)) / 10.0;
            const ks = once.U < 0.8 * Ur ? once.U / (0.8 * Ur) : 1;

            once.ET = Math.min(C[month][part] * ks * PET, once.U + once.P - once.Q);
        }
        //-------------------------------------PC:入滲至深層的水量
        once.PC = Math.max(0, once.U + once.P - once.Q - once.ET - Ur);
        //-------------------------------------G:地下水出水量
        once.G = r * once.S;
        //-------------------------------------update地下水含量
        {
            if (i < data.length - 1) {
                data[i + 1].U = once.U + once.P - once.Q - once.ET - once.PC;
                data[i + 1].S = once.S + once.PC - once.G - 0;
            }
        }


        return once;
    });

    console.log(new Date().getTime() - t1);
    // console.table(data);
    
}