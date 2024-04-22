from django.shortcuts import render
from django.shortcuts import redirect
from django.http import HttpResponse
from django.views.decorators.http import require_POST
from django.contrib import messages
from django.core.mail import EmailMessage
from django.shortcuts import redirect
from django.conf import settings
from .models import Book
import logging
from django.views.decorators.clickjacking import xframe_options_exempt
from django.core.paginator import Paginator

# def index(request):
#     print(request.user)
#     return render(request,'index.html') # Template name
def index(request):
    if not request.session.get('authenticated', False):
        return redirect('password_entry')
    return render(request, 'index.html')

def about(request):
    return render(request,'about.html') # Template name
def reports(request):
    return render(request,'reports.html')
def test(request):
    return render(request,'test.html') # Template name

def contact(request):
    return render(request,'contact.html') # Template name
@xframe_options_exempt
def read_pdf(request):
    pdf_url = request.GET.get('pdf_url', '')
    context = {'pdf_url': pdf_url}
    return render(request, 'read_pdf.html', context)  

logger = logging.getLogger(__name__)  # Set up a logger
@require_POST
def end_session_view(request):

    messages.success(request, "Your session has ended.")  # Add a success message
    return redirect('index')  # Redirect to 'index' named URL pattern

logger = logging.getLogger(__name__)  # Set up a logger

def send_email(request):
    user_email = request.POST.get('email')
    try:
        email = EmailMessage(
            'Hello',
            'Body goes here',
            settings.EMAIL_HOST_USER,
            [user_email],  # Replace with the recipient's email
            reply_to=[settings.EMAIL_HOST_USER]
            # Include attachments as needed
        )
        email.send()
        messages.success(request, "Email sent successfully.")
    except Exception as e:
        # Log the error
        logger.error(f'Error sending email: {e}')
        # Add the error to messages
        messages.error(request, f'Email could not be sent: {e}')
    return redirect('index')  # Redirect to 'home' or any other page you wish


def book_list(request):
    books = Book.objects.all()
    return render(request, 'books.html', {'books': books})


def password_entry(request):
    correct_password = "website123#"  # Set your password here

    if request.method == "POST":
        password = request.POST.get("password", "")
        if password == correct_password:
            request.session['authenticated'] = True  # Set session as authenticated
            return redirect('index')
        else:
            messages.error(request, 'Incorrect password, please try again.')
            return redirect('password_entry')

    return render(request, 'password_entry.html')