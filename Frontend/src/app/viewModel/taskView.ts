export class TaskViewModel {
    id: string;
    description: string;
    user: string; 
    contact: number; 

    constructor(taskData: any) {
        this.id = taskData.id;
        this.description = taskData.description;
        this.user = taskData.user.name; 
        this.contact = taskData.user.contacto; 
    }
}