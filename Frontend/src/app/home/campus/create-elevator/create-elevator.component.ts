import { Component } from '@angular/core';
import {Observable, Subscription} from "rxjs";
import {MatSnackBar} from "@angular/material/snack-bar";
import {FormControl, FormGroup, FormBuilder} from "@angular/forms";
import { FloorDTO, FloorResponseDTO } from "../../../../dto/floorDTO";
import { BuildingResponseDTO } from "../../../../dto/buildingDTO";
import { BuildingService } from "../../../services/building.service";

@Component({
  selector: 'app-create-elevator',
  templateUrl: './create-elevator.component.html',
  styleUrls: ['./create-elevator.component.css']
})
export class CreateElevatorComponent {
  buildingList: BuildingResponseDTO[] = [];
  buildingSelectionControl =  new FormControl();
  buildingServiceSubscription$ = new Subscription();

  floorList: FloorResponseDTO[] = [];
  floorSelectionControl =  new FormControl();
  floorServiceSubscription$ = new Subscription();

  constructor(private buildingService: BuildingService,
    private _snackBar: MatSnackBar,) {}

  ngOnInit(): void {
    // fetch building list from service
    this.buildingServiceSubscription$ = this.buildingService.getAllBuildings().subscribe(
      response => {
        this.buildingList = response;
      },
      error => {
        this._snackBar.open("Unable to get buildings!", "close", {
          duration: 5000,
          panelClass: ['snackbar-warning']
        });
      }
    );
  }

}
