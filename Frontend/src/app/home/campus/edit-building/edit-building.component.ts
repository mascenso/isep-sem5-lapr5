import { Component, OnInit } from '@angular/core';
import { FormBuilder, FormGroup, Validators } from '@angular/forms';
import { BuildingService } from '../../../services/building.service';
import { BuildingResponseDTO } from '../../../../dto/buildingDTO';
import {MatSnackBar} from "@angular/material/snack-bar";

@Component({
  selector: 'app-edit-building',
  templateUrl: './edit-building.component.html',
  styleUrls: ['./edit-building.component.css']
})
export class EditBuildingComponent {

  buildingForm!: FormGroup;
  buildings: BuildingResponseDTO[] = [];
  selectedBuilding: any ;

  constructor(
    private formBuilder: FormBuilder,
    private buildingService: BuildingService,
    private _snackBar: MatSnackBar
  ) {
    this.buildingForm = this.formBuilder.group({
      id: ['', Validators.required],
      code: ['', Validators.required],
      maxWidth: [null],
      maxLength: [null],
      name: [''],
      description: ['']
    });
  }

  ngOnInit(): void {

    this.getBuildings();
  }

  onBuildingSelected(buildingId: string): void {
    //filtra no grupo dos edificios aquele que foi selecionado
    this.selectedBuilding = this.buildings.find(building => building.id === buildingId)
    //atualiza o form que esta a preencher os inputs no frontend
    this.updateBuildingForm()

  }

  updateBuildingForm(){
    this.buildingForm = this.formBuilder.group({
      id: [this.selectedBuilding?.id || '', Validators.required],
      code: [this.selectedBuilding?.code || '', Validators.required],
      maxWidth: [this.selectedBuilding?.maxWidth || null],
      maxLength: [this.selectedBuilding?.maxLength || null],
      name: [this.selectedBuilding?.name || ''],
      description: [this.selectedBuilding?.description || '']
    });
  }

  getBuildings(){
    this.buildingService.getAllBuildings().subscribe((buildings) => {
      this.buildings = buildings;
    });
  }

  onSubmit(): void {
    //Envia para o backend o patch do edificio
    this.buildingService.editBuilding(this.buildingForm.value).subscribe(
      (building) => {

        this.selectedBuilding = building;
        this.updateBuildingForm();
        this.getBuildings();

        this._snackBar.open("Building updated!", "close", {
          duration: 5000,
          panelClass: ['snackbar-success']
        });
    },
    (error) => {

      this._snackBar.open("Error in building update!", "close", {
        duration: 5000,
        panelClass: ['snackbar-error']
      });
    });

  }

}
