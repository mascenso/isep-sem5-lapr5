import { Component } from '@angular/core';
import { FormBuilder, FormGroup, Validators } from '@angular/forms';
import { MatSnackBar } from '@angular/material/snack-bar';
import { BuildingService } from 'src/app/services/building.service';
import { ElevatorService } from 'src/app/services/elevator.service';
import { BuildingResponseDTO } from 'src/dto/buildingDTO';
import { ElevatorResponseDTO } from 'src/dto/elevatorDTO';

@Component({
  selector: 'app-edit-elevators',
  templateUrl: './edit-elevators.component.html',
  styleUrls: ['./edit-elevators.component.css']
})
export class EditElevatorsComponent {
  elevatorForm!: FormGroup;
  buildings: BuildingResponseDTO[] = [];
  selectedBuilding: any;
  selectedElevator: ElevatorResponseDTO | null = null; // Ajustado para um único objeto de elevador

  constructor(
    private formBuilder: FormBuilder,
    private buildingService: BuildingService,
    private elevatorService: ElevatorService,
    private _snackBar: MatSnackBar
  ) {
    this.elevatorForm = this.formBuilder.group({
      id: ['', Validators.required],
      code: [''],
      floorList: this.formBuilder.array([]),
      buildingId: ['']
    });
  }

  ngOnInit(): void {
    this.getBuildings();
  }

  onBuildingSelected(buildingId: string): void {
    this.selectedBuilding = this.buildings.find(building => building.id === buildingId);
    this.getBuildingElevator(buildingId); // Chama o método para buscar um único elevador
  }

  getBuildings() {
    this.buildingService.getAllBuildings().subscribe((buildings) => {
      this.buildings = buildings;
    });
  }

  getBuildingElevator(buildingId: string) {
    this.elevatorService.getBuildingElevators(buildingId).subscribe((elevator) => {
      this.selectedElevator = elevator; // Atribui o elevador retornado
      this.updateElevatorForm(); // Atualiza o formulário com os detalhes do elevador
    });
  }

  updateElevatorForm(){
    this.elevatorForm = this.formBuilder.group({
      id: [this.selectedElevator?.id || '', Validators.required],
      code: [this.selectedElevator?.code || ''],
      floorList: [this.selectedElevator?.floorList || []],
      buildingId: [this.selectedElevator?.buildingId || '']
    });
  }

  onSubmit(): void {
    this.elevatorService.editElevator(this.elevatorForm.value).subscribe(
      () => {
        this.getBuildingElevator(this.selectedBuilding.id); // Requisita o elevador após a edição
        this._snackBar.open("Elevator updated!", "close", {
          duration: 5000,
          panelClass: ['snackbar-success']
        });
      },
      (error) => {
        this._snackBar.open("Error in elevator update!", "close", {
          duration: 5000,
          panelClass: ['snackbar-error']
        });
      });
  }
}
