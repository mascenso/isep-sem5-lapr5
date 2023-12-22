import { Component, OnInit } from '@angular/core';
import { FormBuilder, FormGroup, Validators } from '@angular/forms';
import { BuildingResponseDTO } from "../../../../dto/buildingDTO";
import { FloorResponseDTO } from 'src/dto/floorDTO';
import {BuildingService} from '../../../services/building.service';
import { FloorService } from "../../../services/floor.service";
import { TasksService } from 'src/app/services/tasks.service';
import {MatSnackBar} from "@angular/material/snack-bar";
import {Observable, Subscription} from "rxjs";

@Component({
  selector: 'app-create-task',
  templateUrl: './create-task.component.html',
  styleUrls: ['./create-task.component.css']
})
export class CreateTaskComponent implements OnInit{
  selectedTask!: string;
  selectedBuilding!:string;
  selectedFloor!:string;
  vigilanciaForm!: FormGroup;
  pickupForm!: FormGroup;
  buildings: BuildingResponseDTO[] = [];
  floors: FloorResponseDTO[] = [];
  buildingServiceSubscription$ = new Subscription();
  floorServiceSubscription$ = new Subscription();

  constructor(private fb: FormBuilder, private buildingService: BuildingService,
    private floorService: FloorService,
    private _snackBar: MatSnackBar,
    private taskService: TasksService) { }

  ngOnInit(): void {
    // Inicialize os formulários conforme necessário
    this.vigilanciaForm = this.fb.group({
      description: ['', Validators.required],
      buildingId: ['', Validators.required],
      floors: [[]],
      contactNumber: [null, Validators.required],
      user: this.fb.group({
        userName:[null, Validators.required],
        userContact:[null,Validators.required]}),
      approved: [false]
    });

    this.pickupForm = this.fb.group({
      description: ['', Validators.required],
      pickupLocalization: this.fb.group({
        buildingId: ['', Validators.required],
        floor: [null, Validators.required],
        room: [[]]
      }),
      deliveryLocalization: this.fb.group({
        buildingId: ['', Validators.required],
        floor: [null, Validators.required],
        room: [[]]
      }),
      contactNumber: [null, Validators.required],
      user: [null, Validators.required],
      deliveryContact: this.fb.group({
        name: ['', Validators.required],
        contactNumber: [null, Validators.required]
      }),
      pickupContact: this.fb.group({
        name: ['', Validators.required],
        contactNumber: [null, Validators.required]
      }),
      approved: [false]
    });

       // fetch building list from service
       this.buildingServiceSubscription$ = this.buildingService.getAllBuildings().subscribe(
        response => {
          this.buildings = response;
        },
        error => {
          this._snackBar.open("Unable to get buildings!", "close", {
            duration: 5000,
            panelClass: ['snackbar-warning']
          });
        }
      );
  }

  onBuildingSelector(building:any){
    if (building) {
      this.floorServiceSubscription$ = this.floorService.getFloorsAtBuildings(building, true ).subscribe(
        floorData => {
          this.floors = floorData;
        },
        error => {
          this._snackBar.open(error.error, "close", {
            duration: 5000,
            panelClass: ['snackbar-error']
          });
        }
      )
    }
  }

  onSubmit() {

    if (this.selectedTask === 'vigilancia') {
      let taskForm = this.vigilanciaForm.value;
      taskForm.buildingId = this.selectedBuilding;
      taskForm.floors.push(this.selectedFloor);

      this.taskService.createVigilanceTask(taskForm).subscribe(
        (response) => {
          this._snackBar.open("Tarefa criada com sucesso!", "close", {
            duration: 5000,
            panelClass: ['snackbar-success']
          });
        },
        (error) => {
          this._snackBar.open("Erro a criar a Tarefa!", "close", {
            duration: 5000,
            panelClass: ['snackbar-error']
          });
        }
      );
    }
  }
}
