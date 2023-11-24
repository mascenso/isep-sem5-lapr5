import { Component, OnInit } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { MatSnackBar } from '@angular/material/snack-bar';
import { BuildingService } from '../../../services/building.service';
import { FloorService } from '../../../services/floor.service'; 
import { BuildingResponseDTO } from "../../../../dto/buildingDTO";

@Component({
  selector: 'app-add-map-floor',
  templateUrl: './add-map-floor.component.html',
  styleUrls: ['./add-map-floor.component.css']
})
export class AddMapFloorComponent implements OnInit{

  selectedBuilding: string = '';
  selectedFloor: string = '';
  buildings: BuildingResponseDTO[] = [];
  floors: any[] = [];
  fileToUpload: File | null = null;

  constructor(
    private http: HttpClient,
    private buildingService: BuildingService,
    private floorService: FloorService,
    private snackBar: MatSnackBar
  ) {}

  ngOnInit() {
    this.getAllBuildings();
  }

  getAllBuildings() {
    this.buildingService.getAllBuildings().subscribe(
      (buildings) => {
        this.buildings = buildings;
      },
      (error) => {
        this.snackBar.open('Erro ao obter a lista de buildings:', 'Fechar', { duration: 3000,panelClass:['snackbar-error'] });
        console.error('Erro ao obter a lista de buildings:', error);
      }
    );
  }

  getFloorsAtBuilding(buildingId: string) {
    this.floorService.getFloorsAtBuildings(buildingId).subscribe(
      (floors) => {
        this.floors = floors;
      },
      (error) => {
        this.snackBar.open('Erro ao obter a lista de pisos:', 'Fechar', { duration: 3000, panelClass:['snackbar-error'] });
        console.error('Erro ao obter a lista de pisos:', error);
      }
    );
  }

  onFileSelected(event: any) {
    this.fileToUpload = event;
  }

  submitMap() {

    if (!this.selectedBuilding || !this.selectedFloor || !this.fileToUpload) {
      this.snackBar.open('Por favor, preencha todos os campos.', 'Fechar', { duration: 3000 });
      return;
    }
    
    this.floorService.addMapFloor(this.selectedFloor,this.fileToUpload).subscribe(
      (response) => {
        this.snackBar.open('Mapa criado com sucesso.', 'Fechar', { duration: 3000, panelClass: ['snackbar-success'] });
        console.log('Mapa criado com sucesso', response);
      },
      (error) => {
        this.snackBar.open('Erro ao adicionar mapa', 'Fechar', { duration: 3000,panelClass:['snackbar-error'] });
        console.error('Erro ao adicionar mapa', error);
      }
    );

    this.snackBar.open('Mapa do andar enviado com sucesso.', 'Fechar', { duration: 3000, panelClass: ['snackbar-success'] });
  }
}