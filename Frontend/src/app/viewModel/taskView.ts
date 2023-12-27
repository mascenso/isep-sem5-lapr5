export interface TaskViewModel {
    id: string;
    description: string;
    user: string;
    contact: number;
    type: 'Pickup' | 'Vigilance';
  }