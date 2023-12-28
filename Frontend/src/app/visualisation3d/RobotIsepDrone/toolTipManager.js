import * as THREE from "three";
export default class TooltipManager {
    constructor(scene, camera, renderer) {
        this.scene = scene;
        this.camera = camera;
        this.renderer = renderer;
        this.raycaster = new THREE.Raycaster();
        this.mouse = new THREE.Vector2();
        this.tooltips = [];

        // Adicionar evento de movimento do mouse
        window.addEventListener('mousemove', this.onMouseMove.bind(this), false);
    }

    addTooltip(object, text) {
        object.userData.name = text;
        this.tooltips.push(object);
    }

    onMouseMove(event) {

        // Atualizar as coordenadas do mouse
        this.mouse.x = (event.clientX / window.innerWidth) * 2 - 1;
        this.mouse.y = - (event.clientY / window.innerHeight) * 2 + 1;
        
        // Configurar o raio a partir das coordenadas do mouse
        this.raycaster.setFromCamera(this.mouse, this.camera);

        // Verificar se o raio intersecta os objetos de tooltips
        var intersects = this.raycaster.intersectObjects(this.tooltips,true);

        // Exibir tooltip se houver interseção
        if (intersects.length > 0) {
            for (let i = 0; i < intersects.length; i++) {
                var tooltipText = intersects[i].object.userData.name;
                console.log("Tooltip: " + tooltipText);
                
            }


        }
    }

    update() {
        // Atualizar lógica de atualização, se necessário
    }
}