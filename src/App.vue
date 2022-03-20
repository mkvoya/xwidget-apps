<template>
  <mindmap v-model="data" drag zoom></mindmap>
  <!-- <button v-on:click="sendMsg('hi')"> send msg </button> -->
</template>

<script>
import mindmap from 'vue3-mindmap'
import 'vue3-mindmap/dist/style.css'

export default {
    name: 'MindMap',
    components: {
        mindmap
    },
    data: function () {
        return {
            conn: null,
            data: [{
                "name":"如何学习D3",
                "children": [
                    {
                        "name":"预备知识",
                        "children": [
                            { "name":"HTML & CSS" },
                            { "name":"JavaScript" },
                        ]
                    },
                    {
                        "name":"安装",
                        "collapse": true,
                        "children": [ { "name": "折叠节点" } ]
                    },
                    { "name":"进阶", "left": true },
                ]
            }]
        }
    },
    methods: {
        sendMsg: function (msg) {
            this.conn.send(msg)
        },
        addNode: function (node) {
            this.data[0].children.push({ "name": node })
        }
    },
    created () {
        console.log("Starting the websocket...")
        this.conn = new WebSocket("ws://127.0.0.1:12302")
        console.log(this.conn)

        this.conn.onmessage = function (event) {
            console.log(event)
            this.addNode(event)
        }
        this.conn.onopen = function (event) {
            console.log("Websocket connection established.")
            console.log(event)
        }
    }
}
</script>

<style>
#app {
    font-family: Avenir, Helvetica, Arial, sans-serif;
    -webkit-font-smoothing: antialiased;
    -moz-osx-font-smoothing: grayscale;
    text-align: center;
    color: #2c3e50;
    /* margin-top: 60px; */
    width: 100%;
    height: 100%;
}

mindmap {
    width: 100%;
    height: 100%;
}

html, body {
    height: 100%;
    margin: 0;
    padding: 0;
}
</style>
