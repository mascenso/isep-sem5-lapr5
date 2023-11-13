import {Component, OnDestroy, OnInit} from '@angular/core';
import {FormControl} from "@angular/forms";
import {BuildingResponseDto, BuildingService} from "../../../services/building.service";
import {animate, state, style, transition, trigger} from "@angular/animations";
import {FloorDto, FloorService} from "../../../services/floor.service";
import {MatSnackBar} from "@angular/material/snack-bar";
import {Observable, Subscription} from "rxjs";

@Component({
  selector: 'app-floor-list',
  templateUrl: './floor-list.component.html',
  animations: [
    trigger('detailExpand', [
      state('collapsed,void', style({height: '0px', minHeight: '0'})),
      state('expanded', style({height: '*'})),
      transition('expanded <=> collapsed', animate('225ms cubic-bezier(0.4, 0.0, 0.2, 1)')),
    ]),
  ],
  styleUrls: ['./floor-list.component.css']
})
export class FloorListComponent implements OnInit, OnDestroy {

  buildingList: BuildingResponseDto[] = [];
  buildingSelectionControl =  new FormControl();
  buildingServiceSubscription$ = new Subscription();

  floorServiceSubscription$ = new Subscription();

  dataSource: FloorDto[] = [];
  columnsToDisplay = ['id', 'floorNumber', 'width', 'length'];
  columnsToDisplayWithExpand = [...this.columnsToDisplay, 'expand'];
  expandedElement: FloorDto | null | undefined;

  constructor(private buildingService: BuildingService,
              private floorService: FloorService,
              private _snackBar: MatSnackBar) {}

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
    )
  }

  onSelectionUpdateTable(selection: any): void {
    console.log(selection);
    if (selection) {
      this.floorServiceSubscription$ = this.floorService.getFloorsWithElevatorByBuildingId(selection, true ).subscribe(
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

}

const FLOOR_DATA: FloorDto[] = [
  {
    id: 'floor-id-1',
    buildingId: 'building-id-1',
    width: 10,
    length: 10,
    floorNumber: 1,
    description: `Hydrogen is a chemical element with symbol H and atomic number 1. With a standard
        atomic weight of 1.008, hydrogen is the lightest element on the periodic table.`,
    floorMap: [[0,1,0,1,1],[0,0,0,0,0],[0,1,0,1,1],[0,1,0,1,1],[0,1,0,1,1]]
  },
  {
    id: 'floor-id-2',
    buildingId: 'building-id-2',
    width: 10,
    length: 10,
    floorNumber: 2,
    description: `Hydrogen is a chemical element with symbol H and atomic number 1. With a standard
        atomic weight of 1.008, hydrogen is the lightest element on the periodic table.`,
    floorMap: [[0,1,0,1,1],[0,0,0,0,0],[0,1,0,1,1],[0,1,0,1,1],[0,1,0,1,1]]
  },
  {
    id: 'floor-id-3',
    buildingId: 'building-id-2',
    width: 10,
    length: 10,
    floorNumber: 3,
    description: `Hydrogen is a chemical element with symbol H and atomic number 1. With a standard
        atomic weight of 1.008, hydrogen is the lightest element on the periodic table.`,
    floorMap: [[0,1,0,1,1],[0,0,0,0,0],[0,1,0,1,1],[0,1,0,1,1],[0,1,0,1,1]]
  },
];
