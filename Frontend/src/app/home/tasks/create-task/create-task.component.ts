import { Component, OnInit } from '@angular/core';
import { FormBuilder, FormGroup, Validators, FormControl } from '@angular/forms';
import { BuildingResponseDTO } from "../../../../dto/buildingDTO";
import { FloorResponseDTO } from 'src/dto/floorDTO';
import {BuildingService} from '../../../services/building.service';
import { FloorService } from "../../../services/floor.service";
import { TasksService } from 'src/app/services/tasks.service';
import {MatSnackBar} from "@angular/material/snack-bar";
import {Observable, Subscription} from "rxjs";
import {UserService} from "../../../services/user.service";
import {TaskVigilanceRequestDTO} from "../../../../dto/taskVigilanceDTO"
import {TaskPickupRequestDTO} from "../../../../dto/taskPickupDTO"

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
  pickupFloors: FloorResponseDTO[] = [];
  deliveryFloors: FloorResponseDTO[] = [];
  buildingServiceSubscription$ = new Subscription();
  floorServiceSubscription$ = new Subscription();

  user = {userName:"", userContact:"", userEmail:""}


  constructor(private fb: FormBuilder, private buildingService: BuildingService,
    private floorService: FloorService,
    private _snackBar: MatSnackBar,
    private taskService: TasksService,
    private userService: UserService) { }

  ngOnInit(): void {
    this.userService.getUserData(true).subscribe((response) => {this.user.userName = response.firstName, this.user.userContact = response.phoneNumber, this.user.userEmail = response.email},);

    // Inicialize os formulários conforme necessário
    this.vigilanciaForm = this.fb.group({
      description: ['', Validators.required],
      buildingId: ['', Validators.required],
      floors: [[]],
      startPosition: this.fb.group({
        x: ['', Validators.required], // X start position
        y: ['', Validators.required], // Y start position
      }),
      endPosition: this.fb.group({
        x: ['', Validators.required], // X end position
        y: ['', Validators.required], // Y end position
      }),
      contactNumber: [null, Validators.required],
      user: this.fb.group({
        userName:[null, Validators.required],
        userContact:[null,Validators.required],
        userEmail:[null,Validators.required]}
      ),
      approved: [false],
      pending:[true]
    });

    this.pickupForm = this.fb.group({
      description: ['', Validators.required],
      pickupLocalization: this.fb.group({
        buildingId: ['', Validators.required],
        floor: [null, Validators.required],
        room: this.fb.group({
          x: ['', Validators.required], // X start position
          y: ['', Validators.required], // Y start position
        }),
      }),
      deliveryLocalization: this.fb.group({
        buildingId: ['', Validators.required],
        floor: [null, Validators.required],
        room: this.fb.group({
          x: ['', Validators.required], // X start position
          y: ['', Validators.required], // Y start position
        }),
      }),
      contactNumber: [null, Validators.required],
      user: this.fb.group({
        userName:[null, Validators.required],
        userContact:[null,Validators.required]}),
      deliveryContact: this.fb.group({
        name: ['', Validators.required],
        contactNumber: [null, Validators.required]
      }),
      pickupContact: this.fb.group({
        name: ['', Validators.required],
        contactNumber: [null, Validators.required]
      }),
      approved: [false],
      pending:[true]
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

  onBuildingPickupSelector(building:any){
    if (building) {
      this.floorServiceSubscription$ = this.floorService.getFloorsAtBuildings(building, true ).subscribe(
        floorData => {
          this.pickupFloors = floorData;
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
  onBuildingDeliverySelector(building:any){
    if (building) {
      this.floorServiceSubscription$ = this.floorService.getFloorsAtBuildings(building, true ).subscribe(
        floorData => {
          this.deliveryFloors = floorData;
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
      taskForm.floors[0] =this.selectedFloor;
      taskForm.user = this.user;
      taskForm.startPosition = [taskForm.startPosition.x, taskForm.startPosition.y]
      taskForm.endPosition = [taskForm.endPosition.x, taskForm.endPosition.y]

      this.taskService.createVigilanceTask(taskForm as TaskVigilanceRequestDTO).subscribe(
        (response) => {
          this.vigilanciaForm.reset(),
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
    }else if (this.selectedTask === 'pickup'){

      let taskForm = this.pickupForm.value;
      taskForm.contactNumber = this.user.userContact;
      taskForm.user = this.user;
      taskForm.deliveryLocalization.room = [parseInt(taskForm.deliveryLocalization.room.x,10) , parseInt(taskForm.deliveryLocalization.room.y,10)]
      taskForm.pickupLocalization.room = [parseInt(taskForm.pickupLocalization.room.x,10) , parseInt(taskForm.pickupLocalization.room.y,10)]

      this.taskService.createPickupTask(taskForm as TaskPickupRequestDTO).subscribe(
        (response) => {
          this.pickupForm.reset(),
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
