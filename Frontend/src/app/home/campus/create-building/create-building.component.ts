import { Component } from '@angular/core';
import {FormControl, FormGroup, Validators} from "@angular/forms";
import {BuildingResponseDto, BuildingService, CreateBuildingRequestDto} from "../../../services/building.service";
import {MatSnackBar} from "@angular/material/snack-bar";

@Component({
  selector: 'app-create-building',
  templateUrl: './create-building.component.html',
  styleUrls: ['./create-building.component.css']
})
export class CreateBuildingComponent {
  buildingForm = new FormGroup({
    code: new FormControl('', [Validators.required, Validators.pattern("^[A-Z]{1}[0-9]{4}$")]),
    name: new FormControl(''),
    maxWidth: new FormControl('', [Validators.required, Validators.pattern("^[0-9]*$")]),
    maxLength: new FormControl('', [Validators.required, Validators.pattern("^[0-9]*$")]),
    description: new FormControl('')
  });

  createdBuilding: BuildingResponseDto | undefined;

  constructor(private buildingService: BuildingService,
              private _snackBar: MatSnackBar) {
  }


  public onSubmit() {
    this.buildingService.createBuilding(this.buildingForm.value as CreateBuildingRequestDto).subscribe(
      response => {
          this.createdBuilding = response;
          this._snackBar.open("Building created!", "close", {
            duration: 5000,
            panelClass: ['snackbar-success']
          });
      },
      error => {
          console.log('Error creating building: ', error);
          this._snackBar.open(error.message, "close", {
            duration: 5000,
            panelClass: ['snackbar-error']
          });
      }
    );
  }

}
