import {Component, OnDestroy, OnInit} from '@angular/core';
import {FormControl, FormGroup, FormBuilder} from "@angular/forms";
import {animate, state, style, transition, trigger} from "@angular/animations";
import {MatSnackBar} from "@angular/material/snack-bar";
import {Observable, Subscription} from "rxjs";
import { Router } from '@angular/router';
import { FloorDTO, FloorResponseDTO } from "../../../../dto/floorDTO";
import { BuildingResponseDTO } from "../../../../dto/buildingDTO";
import { BuildingService } from "../../../services/building.service";
import { FloorService } from "../../../services/floor.service";

@Component({
  selector: 'app-edit-floors',
  templateUrl: './edit-floors.component.html',
  animations: [
    trigger('detailExpand', [
      state('collapsed,void', style({height: '0px', minHeight: '0'})),
      state('expanded', style({height: '*'})),
      transition('expanded <=> collapsed', animate('225ms cubic-bezier(0.4, 0.0, 0.2, 1)')),
    ]),
  ],
  styleUrls: ['./edit-floors.component.css']
})
export class EditFloorsComponent {
  showForm = false; // Initially, the form is hidden
  buildingList: BuildingResponseDTO[] = [];
  buildingSelectionControl =  new FormControl();
  buildingServiceSubscription$ = new Subscription();


  floorList: FloorResponseDTO[] = [];
  floorSelectionControl =  new FormControl();
  floorServiceSubscription$ = new Subscription();


  dataSource: FloorDTO[] = [];
  columnsToDisplay = ['id', 'floorNumber', 'width', 'length'];
  columnsToDisplayWithExpand = [...this.columnsToDisplay, 'expand'];
  expandedElement: FloorDTO | null | undefined;

  selectedFloor: FloorDTO | undefined;

  floorForm = new FormGroup({
    Width: new FormControl<number>(0),
    Length: new FormControl<number>(0),
    FloorNumber: new FormControl<number>(0),
    description: new FormControl(''),
  });

  constructor(private buildingService: BuildingService,
              private floorService: FloorService,
              private _snackBar: MatSnackBar,
              private router: Router) {}

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

  onSelectionUpdateForm(selection: any): void {
    if (selection) {
      this.floorServiceSubscription$ = this.floorService.getFloorsAtBuildings(selection, true ).subscribe(
        floorData => {
          this.dataSource = floorData;
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

  ngOnDestroy(): void {
    this.buildingServiceSubscription$.unsubscribe();
    this.floorServiceSubscription$.unsubscribe();
  }

  onEdit() {
    this.floorForm = new FormGroup({
      Width: new FormControl<number>(this.selectedFloor ? this.selectedFloor.width : 0),
      Length: new FormControl<number>(this.selectedFloor ? this.selectedFloor.length : 0),
      FloorNumber: new FormControl<number>(this.selectedFloor ? this.selectedFloor.floorNumber : 0),
      description: new FormControl(this.selectedFloor ? this.selectedFloor.description : ''),
    });
    console.log('Edit button clicked');
    if(this.showForm == false) {
      this.showForm = true;
    } else {
      this.showForm = false;
    }
  }

  onFloorSelected(selectedFloorId: any): void {
    this.selectedFloor = this.dataSource.find(floor => floor.id === selectedFloorId);
  }

  onUpdate() {
    console.log('Update button clicked');
  
    if (this.selectedFloor) {
      const updatedFloorData = {
        id: this.selectedFloor.id,
        buildingId: this.selectedFloor.buildingId,
        width: null ? this.selectedFloor : this.floorForm.value.Width,
        length: this.selectedFloor.length ? this.floorForm.value.Length : this.selectedFloor.length,
        floorNumber: this.floorForm.value.FloorNumber,
        description: this.floorForm.value.description,
      };

      console.log(this.floorForm.value.FloorNumber);
      console.log(updatedFloorData);

      this.floorService.updateFloor(updatedFloorData as FloorDTO).subscribe(
        response => {
          this.dataSource = response;
          this._snackBar.open("floor updated!", "close", {
            duration: 5000,
            panelClass: ['snackbar-success']
          });
        },
        error => {
          console.log('Error editing floor: ', error);
          this._snackBar.open(error.message, "close", {
            duration: 5000,
            panelClass: ['snackbar-error']
          });
        }
      );
      this.showForm = false;
    }
  }

  onCancel() {
    return this.router.navigate(['../home/campus']);
  }

}
