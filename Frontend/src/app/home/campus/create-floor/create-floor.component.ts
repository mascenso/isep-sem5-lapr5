import { Component,OnInit } from '@angular/core';
import { FormBuilder, FormGroup, Validators } from '@angular/forms';
import {BuildingService} from '../../../services/building.service'
import { BuildingResponseDTO } from "../../../../dto/buildingDTO";
import {FloorService} from "../../../services/floor.service";
import {MatSnackBar} from "@angular/material/snack-bar";

@Component({
  selector: 'app-create-floor',
  templateUrl: './create-floor.component.html',
  styleUrls: ['./create-floor.component.css']
})
export class CreateFloorComponent implements OnInit{

  buildings: BuildingResponseDTO[] = [];
  selectedBuilding: string = "";
  floorForm!: FormGroup;

  constructor(private buildingService: BuildingService, private fb: FormBuilder, private floorService: FloorService, private _snackBar: MatSnackBar) {}

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
        this._snackBar.open("Piso criado com sucesso!", "close", {
          duration: 5000,
          panelClass: ['snackbar-success']
        });
      },
      (error) => {
        this._snackBar.open("Erro a criar piso!", "close", {
          duration: 5000,
          panelClass: ['snackbar-error']
        });
      }
    );
    
  }

}
