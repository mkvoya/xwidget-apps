<template>
  <img alt="Vue logo" src="./assets/logo.png">
  <mindmap v-model="data"></mindmap>
</template>

<script>
import mindmap from 'vue3-mindmap'
import 'vue3-mindmap/dist/style.css'

export default {
    name: 'MindMap',
    components: {
        mindmap
    },
    setup () => {
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

    return { data }
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
  margin-top: 60px;
}
</style>
