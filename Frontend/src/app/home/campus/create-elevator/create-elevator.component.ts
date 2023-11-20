import { Component } from '@angular/core';
import {Observable, Subscription} from "rxjs";
import {MatSnackBar} from "@angular/material/snack-bar";
import {FormControl, FormGroup, FormArray} from "@angular/forms";
import { FloorDTO, FloorResponseDTO } from "../../../../dto/floorDTO";
import { BuildingResponseDTO } from "../../../../dto/buildingDTO";
import { BuildingService } from "../../../services/building.service";
import { FloorService } from "../../../services/floor.service";
import { ElevatorService } from "../../../services/elevator.service";
import { ElevatorResponseDTO, CreateElevatorDTO } from 'src/dto/elevatorDTO';

@Component({
  selector: 'app-create-elevator',
  templateUrl: './create-elevator.component.html',
  styleUrls: ['./create-elevator.component.css']
})
export class CreateElevatorComponent {

  elevatorForm = new FormGroup({
    code: new FormControl(''),
    floorList: new FormArray([]),
    buildingId: new FormControl('')
  });

  codeSelectionControl = new FormControl();

  buildingList: BuildingResponseDTO[] = [];
  buildingSelectionControl =  new FormControl();
  buildingServiceSubscription$ = new Subscription();

  dataSource: FloorDTO[] = [];
  floorList: FloorResponseDTO[] = [];
  floorSelectionControl =  new FormControl();
  floorServiceSubscription$ = new Subscription();

  createdElevator: ElevatorResponseDTO | undefined;

  constructor(private buildingService: BuildingService,
    private floorService: FloorService,
    private elevatorService: ElevatorService,
    private _snackBar: MatSnackBar,) {}

    ngOnInit(): void {
      this.buildingServiceSubscription$ = this.buildingService.getAllBuildings().subscribe(
        response => {
          this.buildingList = response;
        },
  
      )
    }

    onSelectionUpdateSelectionControl($event: any) {
      if (!$event) {
        this.floorList = [];
        return;
      }
  
      this.floorServiceSubscription$ = this.floorService.getFloorsAtBuildings($event, true).subscribe(
        response => {
          this.floorList = response;
        },
        error => {
          this._snackBar.open("Unable to get floors!", "close", {
            duration: 5000,
            panelClass: ['snackbar-warning']
          });
        }
      );
  
    }

  public onSubmit() {
    this.elevatorForm.patchValue({
      code: this.codeSelectionControl.value,
      floorList: this.floorSelectionControl.value,
      buildingId: this.buildingSelectionControl.value
    });
    this.elevatorService.createElevator(this.elevatorForm.value as CreateElevatorDTO, true).subscribe(
      response => {
          this.createdElevator = response;
          this._snackBar.open("Elevator created!", "close", {
            duration: 5000,
            panelClass: ['snackbar-success']
          });
      },
      error => {
          console.log('Error creating Elevator: ', error);
          this._snackBar.open(error.message, "close", {
            duration: 5000,
            panelClass: ['snackbar-error']
          });
      }
    );
  }

}
