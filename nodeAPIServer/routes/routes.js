var appRouter = function(app) {
    app.get("/", function(req, res) {
        res.send("Hello World");
    });
    app.get("/test", function(req, res) {
        res.send("simple test");
    })
    app.get("/ghost", (req, res) => {
        let n = 3;
        if(req.get("number")) {
            n = req.get("number");
        }
        let ghosts = [];
        for(let i=0; i<n; i++) {
            let x1 = Math.floor(Math.random() * 100);
            let y1 = Math.floor(Math.random() * 100);
            ghosts.push({"x": x1, "y": y1});
        }
        res.send(ghosts);
    })
}

module.exports = appRouter;
