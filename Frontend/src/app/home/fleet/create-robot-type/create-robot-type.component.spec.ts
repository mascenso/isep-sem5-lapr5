import { ComponentFixture, TestBed } from '@angular/core/testing';
import { AuthService } from 'src/app/services/auth.service'
import { CreateRobotTypeComponent } from './create-robot-type.component';
import { HttpClientModule } from '@angular/common/http';
import { MatInputModule } from '@angular/material/input';
import { MatSnackBarModule } from '@angular/material/snack-bar';
import { MatCardModule } from '@angular/material/card';
import { MatSelectModule } from '@angular/material/select';
import { ReactiveFormsModule } from '@angular/forms';
import { FormsModule } from '@angular/forms';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';


beforeEach(() => {
  TestBed.configureTestingModule({
    imports: [
      HttpClientModule,
      MatInputModule,
      MatSnackBarModule,
      MatCardModule,
      MatSelectModule,
      ReactiveFormsModule,
      FormsModule,
      HttpClientTestingModule,
      BrowserAnimationsModule,
    ],
    declarations: [CreateRobotTypeComponent],
    providers: [AuthService],
  });
});




describe('CreateRobotTypeComponent', () => {
  let component: CreateRobotTypeComponent;
  let fixture: ComponentFixture<CreateRobotTypeComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [CreateRobotTypeComponent],
      imports: [ReactiveFormsModule],
    }).compileComponents();
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(CreateRobotTypeComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
