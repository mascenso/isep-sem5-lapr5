import { Component,OnInit } from '@angular/core';
import { FormBuilder, FormGroup, Validators } from '@angular/forms';
import {BuildingService} from '../../../services/building.service'
import { BuildingResponseDTO } from "../../../../dto/buildingDTO";
import {FloorService} from "../../../services/floor.service"

@Component({
  selector: 'app-create-floor',
  templateUrl: './create-floor.component.html',
  styleUrls: ['./create-floor.component.css']
})
export class CreateFloorComponent implements OnInit{

  buildings: BuildingResponseDTO[] = [];
  selectedBuilding: string = "";
  floorForm!: FormGroup;

  constructor(private buildingService: BuildingService, private fb: FormBuilder, private floorService: FloorService) {}

  ngOnInit() {
    this.buildingService.getAllBuildings().subscribe((buildings) => {
      this.buildings = buildings;
    });

    this.floorForm = this.fb.group({
      buildingId: ['', Validators.required],
      width: ['', Validators.required],
      length: ['', Validators.required],
      floorNumber: ['', Validators.required],
      description: [''],
      floorMap: {}, 
    });
  }

  onSubmit() {
    let floorData = this.floorForm.value;
    floorData.buildingId = this.selectedBuilding;

    this.floorService.createFloor(floorData).subscribe(
      (response) => {
        
        console.log('Piso criado com sucesso', response);
      },
      (error) => {
        
        console.error('Erro ao criar piso', error);
      }
    );
    
  }

}
