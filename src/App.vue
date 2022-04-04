<template>
  <mindmap v-model="data" drag zoom edit></mindmap>
  <!-- <button v-on:click="sendMsg('hi')"> send msg </button> -->
</template>

<script>
import mindmap from 'vue3-mindmap'
import 'vue3-mindmap/dist/style.css'
import { defineComponent } from 'vue'

export default defineComponent({
    name: 'App',
    components: {
        mindmap
    },
    setup() {
        const data = [{
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
        return {
            conn: null,
            data: data
        }
    },
    methods: {
        sendMsg: function (msg) {
            this.conn.send(msg)
        },
        setData: function (data) {
            this.data = data
            console.log(this.mindmap)
            this.$forceUpdate()
            // this.$emit('mmdata', new ImData(cloneDeep(this.data[0]), 10, 10, getSize))
            console.log(data)
        },
        addNode: function (node) {
            this.data[0].children.push({ "name": node })
        }
    },
    created () {
        console.log("Starting the websocket...")
        this.conn = new WebSocket("ws://127.0.0.1:12302")
        console.log(this.conn)

        let vm = this;

        this.conn.onmessage = function (event) {
            console.log(event)
            if (event.data.length) {
                console.log(event.data)
                let obj = JSON.parse(event.data)
                console.log(obj)
                vm.setData(obj)
            }
        },
        this.conn.onopen = function (event) {
            console.log("Websocket connection established.")
            console.log(event)
        }
    }
})
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
