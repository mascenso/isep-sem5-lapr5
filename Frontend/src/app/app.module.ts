import { NgModule } from '@angular/core';
import { BrowserModule } from '@angular/platform-browser';
import {FormsModule, ReactiveFormsModule} from '@angular/forms';
import { AppRoutingModule } from './app-routing.module';
import { AppComponent } from './app.component';
import { LoginComponent } from './login/login.component';
import {HTTP_INTERCEPTORS, HttpClientModule} from '@angular/common/http';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';
import { HomeComponent } from './home/home.component';
import { MatToolbarModule } from '@angular/material/toolbar';
import { MatButtonModule } from '@angular/material/button';
import { MatSidenavModule } from '@angular/material/sidenav';
import { MatIconModule } from '@angular/material/icon';
import { MatListModule } from '@angular/material/list';
import { CampusComponent } from './home/campus/campus.component';
import { FleetComponent } from './home/fleet/fleet.component';
import { TasksComponent } from './home/tasks/tasks.component';
import {LoginService} from "./services/loginService.service";
import {MatCardModule} from "@angular/material/card";
import {MatInputModule} from "@angular/material/input";
import {MatPaginatorModule} from "@angular/material/paginator";
import {MatTableModule} from "@angular/material/table";
import {MatSortModule} from "@angular/material/sort";
import { CreateBuildingComponent } from './home/campus/create-building/create-building.component';
import {MatRippleModule} from "@angular/material/core";
import {MatSnackBarModule} from "@angular/material/snack-bar";
import { CreateRobotTypeComponent } from './home/fleet/create-robot-type/create-robot-type.component';
import { MatSelectModule } from '@angular/material/select';
import {MatProgressBarModule} from "@angular/material/progress-bar";
import {ServiceInterceptor} from "./interceptors/service.interceptor";
import { FloorListComponent } from './home/campus/floor-list/floor-list.component';
import { BridgeListComponent } from "./home/campus/bridge-list/bridge-list.component";

@NgModule({
  declarations: [
    AppComponent,
    LoginComponent,
    HomeComponent,
    CampusComponent,
    FleetComponent,
    TasksComponent,
    CreateBuildingComponent,
    CreateRobotTypeComponent,
    FloorListComponent,
    BridgeListComponent
  ],
  imports: [
    BrowserModule,
    AppRoutingModule,
    FormsModule,
    HttpClientModule,
    BrowserAnimationsModule,
    MatToolbarModule,
    MatButtonModule,
    MatSidenavModule,
    MatIconModule,
    MatInputModule,
    MatCardModule,
    MatPaginatorModule,
    MatTableModule,
    MatSortModule,
    MatListModule,
    MatRippleModule,
    ReactiveFormsModule,
    MatSnackBarModule,
    MatProgressBarModule,
    MatSelectModule
  ],
  providers: [{
    provide: HTTP_INTERCEPTORS,
    useClass: ServiceInterceptor,
    multi: true,
  },
  LoginService],
  bootstrap: [AppComponent]
})
export class AppModule { }
