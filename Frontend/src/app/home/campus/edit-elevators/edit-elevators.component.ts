import { Component } from '@angular/core';
import { FormBuilder, FormGroup, Validators } from '@angular/forms';
import { MatSnackBar } from '@angular/material/snack-bar';
import { ElevatorService } from 'src/app/services/elevator.service';
import { ElevatorResponseDTO } from 'src/dto/elevatorDTO';

@Component({
  selector: 'app-edit-elevators',
  templateUrl: './edit-elevators.component.html',
  styleUrls: ['./edit-elevators.component.css']
})
export class EditElevatorsComponent {

  elevatorForm!: FormGroup;
  elevators: ElevatorResponseDTO[] = [];
  selectedElevator: any ;

  constructor(
    private formBuilder: FormBuilder,
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
    this.getElevators();
  }

  onElevatorSelected(elevatorId: string): void {
    //filtra no grupo dos elevadores aquele que foi selecionado
    this.selectedElevator = this.elevators.find(elevator => elevator.id === elevatorId)
    //atualiza o form que esta a preencher os inputs no frontend
    this.updatElevatorForm()

  }

  updatElevatorForm(){
    this.elevatorForm = this.formBuilder.group({
      id: [this.selectedElevator?.id || '', Validators.required],
      code: [this.selectedElevator?.code || ''],
      floorList: [this.selectedElevator?.floorList || []],
      buildingId: [this.selectedElevator?.buildingId || '']
    });
  }


  getElevators(){
    this.elevatorService.getAllElevators().subscribe((elevators) => {
      this.elevators = elevators;
    });
  }

  onSubmit(): void {
    //Envia para o backend o patch do eleavtor
    this.elevatorService.editElevator(this.elevatorForm.value).subscribe(
      (elevator) => {
        this.selectedElevator = elevator;
        this.updatElevatorForm();
        this.getElevators();

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
